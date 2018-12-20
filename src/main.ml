(** A maxpat file is a json file. *)

open Batteries
open Printf
open Graph
open Flowgraph
open Max_parser
open Pd_lexer
open Audiograph_lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error_pd lexbuf =
  let state = ref Pd_lexer.Commands in
  try Pd_parser.prog (Pd_lexer.read state) lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit(-1)
  | Pd_parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)



let output_graph filename graph =
  let file = Pervasives.open_out_bin (filename ^".dot") in
  Dot.output_graph file graph

let main() =
  (*if Array.length Sys.argv < 1 then
      begin
        print_endline "Usage: ims-analysis file-name";
        exit 1;
      end;*)
  let open BatOptParse in
  let output_dot = StdOpt.store_true () in
  let downsample = StdOpt.store_true () in
  let exhaustive = StdOpt.store_true () in
  let debug = StdOpt.store_true () in
  let resamplerDuration = StdOpt.float_option ~default:0. () in
  let deadline = StdOpt.float_option ~default:0. () in (*Find out the period of the audio callback with sane parameters *)
  let stats = StdOpt.store_true () in
  let optparser = OptParser.make ~version:"0.1" ~prog:"ims_analysis"
      ~description:"Make analysis and optimizations of IMS programs" ()
      ~usage:"%prog [options] input_file"
  in
  let display = OptParser.add_group optparser ~description:"Display options" "Display" in
  OptParser.add optparser ~group:display ~help:"Outputs a dot file of the signal processing graph" ~short_name:'d' ~long_name:"dot" output_dot;
  OptParser.add optparser ~group:display ~help:"Stats about the processing" ~short_name:'s' ~long_name:"statistics" stats;
  let optimizations = OptParser.add_group optparser ~description:"Various optimizations" "Optimizations" in
  OptParser.add optparser ~group:optimizations ~help:"Optimization by downsampling" ~short_name:'w' ~long_name:"downsample" downsample;
  let downsampling_opt = OptParser.add_group optparser ~parent:optimizations "Downsampling tweaking" in
  OptParser.add optparser ~group:downsampling_opt ~help:"Deadline of the audio callback in ms" ~short_name:'a' ~long_name:"deadline" deadline;
  OptParser.add optparser ~group:downsampling_opt ~help:"Duration of a resampler in ms" ~short_name:'r' ~long_name:"resampler-dur" resamplerDuration;
  OptParser.add optparser ~group:downsampling_opt ~help:"Exhaustive exploration" ~short_name:'x' ~long_name:"exhaustive" exhaustive;
  OptParser.add optparser ~help:"Debug messages" ~long_name:"debug" debug;


  let remaining_args = OptParser.parse_argv optparser in

  if List.is_empty remaining_args then
    begin
      OptParser.usage optparser ();
      OptParser.error optparser "Missing input file."
  end;

  let filename = List.hd remaining_args in


  let graph = if String.ends_with filename ".maxpat" then
    begin
    Max_parser.parse_maxpat filename
    end
  else if String.ends_with filename ".pd" then
    begin
      if Opt.get debug then
        begin
        let f = File.open_in filename in
        Pd_lexer.channel_to_tokens f
        end;
      let f = File.open_in filename in
      let lexbuf = Lexing.from_channel f  in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let patch = parse_with_error_pd lexbuf in
      print_endline (Puredata.show_patch patch);
      Puredata.build_graph patch
    end
  else if String.ends_with filename ".ag" then
    begin
      if Opt.get debug then
        begin
        let f = File.open_in filename in
        Audiograph_lexer.channel_to_tokens f
        end;
      let f = File.open_in filename in
      let lexbuf = Lexing.from_channel f  in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let nodes,edges, deadl = Audiograph_lexer.parse_with_error_ag lexbuf in
      let resamplerDur = Option.map_default (fun v -> Option.default None (Some Flowgraph.(v.wcet))) (Some (Opt.get resamplerDuration)) (Downsampling.pick_resampler nodes) in
      Opt.set resamplerDuration (Option.default (Opt.get resamplerDuration) resamplerDur);
      Opt.set resamplerDuration (Option.default (Opt.get deadline) deadl);
      Flowgraph.build_graph nodes edges
    end
  else
    begin
    OptParser.usage optparser ();
    OptParser.error optparser "Wrong input format. Expecting: .pd ; .maxpat ; .ag";
    exit 1
    end
  in
  let graph = if Opt.get downsample then
      begin
        print_endline "Downsampling... implementing";
        let durations node  =
          let label = G.V.label node in
          label.wcet |? 0.
        in
        if Opt.get exhaustive then
          begin
            let degraded_versions = Enumeration.enumerate_degraded_versions_vertex (Enumeration.flowgraph_to_graphflow graph) in
            (*List.iter (fun g -> Printf.printf "%s\n" (Enumeration.G.format_graph g)) degraded_versions;*)
            let degraded_versions = List.map Enumeration.graph_to_flowgraph degraded_versions in
            let qu_co = List.map Quality.quality_cost degraded_versions in
            let q_max = ref 0. and cost_min = ref max_float in
            let q_i = ref 0 and cost_i = ref 0 in
            List.iteri (fun i (q, c) -> if !q_max < q then (q_max := q; q_i := i); if !cost_min > c then (cost_min := c ; cost_i := i) ) qu_co;
            Printf.printf "Explored %d degraded versions\n" (List.length degraded_versions);
            Printf.printf "\tBest quality %f, for graph %d\n" !q_max !q_i;
            Printf.printf "\tMinimum cost %f, for graph %d\n" !cost_min !cost_i;
            List.iter (fun (q,c) -> Printf.printf "(%f,%f) " q c) qu_co;
            Printf.printf "\n";
            if Opt.get output_dot then
              begin
                Printf.printf "Outputing all the degraded versions to dot files. \n";
                List.iteri (fun i graph -> output_graph (filename^ "-ex-" ^ (string_of_int i)) graph) degraded_versions
              end;
            (*There is at least one, the original graph *)
            List.hd degraded_versions
          end
        else
          begin
          Downsampling.downsample_components graph durations (Opt.get resamplerDuration) (Opt.get deadline);
          graph
          end
      end
    else graph
  in
  if Opt.get stats then
    if Opt.get downsample then
      begin
        Printf.printf "Number of resamplers: %d\n" (Downsampling.nb_resamplers graph);
        let q, c = Quality.quality_cost graph in
        Printf.printf "Quality: %f and cost: %f\n" q c
      end;
  if Opt.get output_dot then output_graph filename graph;
  print_endline "Processing finished"

  let () = main()
