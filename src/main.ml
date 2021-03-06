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
    raise (SyntaxError msg)
  | Pd_parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    raise Pd_parser.Error



let output_graph filename graph =
  let file = Pervasives.open_out_bin (filename ^".dot") in
  Dot.output_graph file graph;
  Pervasives.close_out file

let report output_name qualities_costs nb_resamplers =
  let rows = List.map2 (fun (q,c) n -> [string_of_float q; string_of_float c; string_of_int n]) qualities_costs nb_resamplers in
  let csv = List.mapi (fun i r -> (string_of_int i)::r ) rows in
  let header = [["Name";"Quality"; "Cost"; "NbResamplers"]] in
  let csv = header @ csv in
  Csv.save ~separator:'\t' output_name csv


let run_exhaustive_downsampling source_graph basename dot audiograph reporting stats merge_resamplers =
  let open BatOptParse in
  let degraded_versions = Enumeration.enumerate_degraded_versions_vertex (Enumeration.flowgraph_to_graphflow source_graph) in
  (*List.iter (fun g -> Printf.printf "%s\n" (Enumeration.G.format_graph g)) degraded_versions;*)
  let degraded_versions = List.map Enumeration.graph_to_flowgraph degraded_versions in
  List.iter Downsampling.check_resamplers degraded_versions;
  let degraded_versions = if Opt.get merge_resamplers then List.mapi (fun i g ->  Printf.printf "graph %d\n" i ; Downsampling.merge_resamplers g) degraded_versions else degraded_versions in
  let nb_degraded_versions = (List.length degraded_versions) - 1 in
  Printf.printf "Explored %d degraded versions\n" nb_degraded_versions;
  if Opt.get dot then
    begin
      Printf.printf "Outputing all the versions to dot files. \n";
      List.iteri (fun i graph -> output_graph (basename ^ "-ex-" ^ (string_of_int i)) graph) degraded_versions
    end;
  if Opt.get audiograph then
    begin
      Printf.printf "Outputing all the versions to audiograph files. \n";
      List.iteri (fun i graph -> Audiograph_export.export (basename ^ "-ex-" ^ (string_of_int i)) graph) degraded_versions
    end;
  let nb_resamplers = List.map Downsampling.nb_resamplers degraded_versions in
  let qu_co = List.map Quality.quality_cost degraded_versions in
  let q_max = ref 0. and cost_min = ref max_float in
  let q_i = ref 0 and cost_i = ref 0 in
  List.iteri (fun i (q, c) -> if !q_max < q then (q_max := q; q_i := i); if !cost_min > c then (cost_min := c ; cost_i := i) ) qu_co;
  Printf.printf "\tBest quality %f, for graph %d\n" !q_max !q_i;
  Printf.printf "\tMinimum cost %f, for graph %d\n" !cost_min !cost_i;
  if Opt.get stats then
    begin
      let avg_nb_resamplers = List.favg (List.map float_of_int nb_resamplers) in
      let max_resamplers = List.max nb_resamplers in
      let open Statistics in
      let statistics = avg_stats (List.map compute_stats degraded_versions) in
      Printf.printf "Average number of resamplers: %f\n" avg_nb_resamplers;
      Printf.printf "Maximum number of resamplers: %d\n" max_resamplers;
      Printf.printf "Statistics: %s\n" (show_stats statistics);
    end;
  if nb_degraded_versions < 10 then
    begin
    List.iter (fun (q,c) -> Printf.printf "(%f,%f) " q c) qu_co;
    Printf.printf "\n"
    end;
  if Opt.get reporting then
    begin
      Printf.printf "Outputing report for all the versions of audiographs. \n";
      report (basename ^ "-theo.csv") qu_co nb_resamplers
    end;
  (*There is at least one, the original graph
    TODO: we should rather return the best one *)
  List.hd degraded_versions

let load_graph debug connect_subpatches resamplerDuration deadline filename =
  let open BatOptParse in
  if String.ends_with filename ".maxpat" then
    begin
      Printf.printf "File: %s \n" filename;
      Some (Max_parser.parse_maxpat filename )
    end
  else if String.ends_with filename ".pd" then
    begin
      Printf.printf "File: %s \n" filename;
      if Opt.get debug then
        begin
          let f = File.open_in filename in
          Pd_lexer.channel_to_tokens f;
          IO.close_in f
        end;
      let f = File.open_in filename in
      let lexbuf = Lexing.from_channel f  in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let patch = parse_with_error_pd lexbuf in
      IO.close_in f;
      (*print_endline (Puredata.show_patch patch);*)
      Some (Puredata.build_graph ~keep_orphans:false ~connect_subpatches:(Opt.get connect_subpatches) patch)
    end
  else if String.ends_with filename ".ag" then
    begin
      Printf.printf "File: %s \n" filename;
      if Opt.get debug then
        begin
          let f = File.open_in filename in
          Audiograph_lexer.channel_to_tokens f;
          IO.close_in f
        end;
      let f = File.open_in filename in
      let lexbuf = Lexing.from_channel f  in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let nodes,edges, deadl = Audiograph_lexer.parse_with_error_ag lexbuf in
      IO.close_in f;
      let resamplerDur = Option.map_default (fun v -> Option.default None (Some Flowgraph.(v.wcet))) (Some (Opt.get resamplerDuration)) (Downsampling.pick_resampler nodes) in
      Opt.set resamplerDuration (Option.default (Opt.get resamplerDuration) resamplerDur);
      Opt.set resamplerDuration (Option.default (Opt.get deadline) deadl);
      Some (Flowgraph.build_graph nodes edges)
    end
  else None

let main() =
  (*if Array.length Sys.argv < 1 then
      begin
        print_endline "Usage: ims-analysis file-name";
        exit 1;
      end;*)
  let open BatOptParse in
  let output_dot = StdOpt.store_true () in
  let output_audiograph = StdOpt.store_true () in
  let output_name = StdOpt.str_option () in
  let downsample = StdOpt.store_true () in
  let exhaustive = StdOpt.store_true () in
  let random = StdOpt.store_true () in
  let nb_samples = StdOpt.int_option ~default:50 () in
  let merge_resamplers = StdOpt.store_true () in
  let debug = StdOpt.store_true () in
  let resamplerDuration = StdOpt.float_option ~default:0. () in
  let deadline = StdOpt.float_option ~default:0. () in (*Find out the period of the audio callback with sane parameters *)
  let stats = StdOpt.store_true () in
  let nb_nodes = StdOpt.int_option () in
  let edge_p = StdOpt.float_option ~default:0.5 () in
  let node_file = StdOpt.str_option ~default:"nodes.ag" () in
  let connect_subpatches = StdOpt.store_true () in
  let report_graphs = StdOpt.store_true () in
  let use_graphs = StdOpt.store_true () in

  let optparser = OptParser.make ~version:"0.1" ~prog:"ims_analysis"
      ~description:"Make analysis and optimizations of IMS programs" ()
      ~usage:"%prog [options] [input_file]"
  in
  let input_options = OptParser.add_group optparser ~description:"Input options" "Input" in
  OptParser.add optparser ~group:input_options ~help:"Connects subpatches to get only one connected graph, not several components per subpatch"  ~long_name:"connect-subpatches" connect_subpatches;
  let display = OptParser.add_group optparser ~description:"Display options" "Display" in
  OptParser.add optparser ~group:display ~help:"Name of the output file (without extension)" ~short_name:'o' ~long_name:"output-name" output_name;
  OptParser.add optparser ~group:display ~help:"Outputs a dot file of the signal processing graph" ~short_name:'d' ~long_name:"dot" output_dot;
  OptParser.add optparser ~group:display ~help:"Outputs an audiograph file of the signal processing graph" ~short_name:'e' ~long_name:"audiograph" output_audiograph;
  OptParser.add optparser ~group:display ~help:"Stats about the processing and the graphs" ~short_name:'s' ~long_name:"stats" stats;
  OptParser.add optparser ~group:display ~help:"A report about the optimization process" ~short_name:'r' ~long_name:"report" report_graphs;
  let optimizations = OptParser.add_group optparser ~description:"Various optimizations" "Optimizations" in
  OptParser.add optparser ~group:optimizations ~help:"Optimization by downsampling" ~short_name:'w' ~long_name:"downsample" downsample;
  let downsampling_opt = OptParser.add_group optparser ~parent:optimizations "Downsampling tweaking" in
  OptParser.add optparser ~group:downsampling_opt ~help:"Deadline of the audio callback in ms" ~short_name:'a' ~long_name:"deadline" deadline;
  OptParser.add optparser ~group:downsampling_opt ~help:"Duration of a resampler in ms" ~short_name:'m' ~long_name:"resampler-dur" resamplerDuration;
  OptParser.add optparser ~group:downsampling_opt ~help:"Exhaustive exploration" ~short_name:'x' ~long_name:"exhaustive" exhaustive;
  OptParser.add optparser ~group:downsampling_opt ~help:"Random exploration" ~short_name:'l' ~long_name:"random" random;
  OptParser.add optparser ~group:downsampling_opt ~help:"Merging resampler optimization"  ~long_name:"merge-resamplers" merge_resamplers;
  OptParser.add optparser ~group:downsampling_opt ~help:"Number of samples"  ~long_name:"nb-samples" nb_samples;
  OptParser.add optparser ~group:downsampling_opt ~help:"From existing audio graphs" ~short_name:'z' ~long_name:"--use-graphs" use_graphs;
  OptParser.add optparser ~group:downsampling_opt ~help:"Number of nodes in case of enumerating/random generation all connected directed graphs with n nodes" ~short_name:'n' ~long_name:"nb-nodes" nb_nodes;
  OptParser.add optparser ~group:downsampling_opt ~help:"Edge probability in case of random generation of connected directed graphs with n nodes" ~short_name:'p' ~long_name:"edge-prob" edge_p;
  OptParser.add optparser ~group:downsampling_opt ~help:"Definitions of possible nodes for use for full enumeration." ~long_name:"node-file" node_file;
  OptParser.add optparser ~help:"Debug messages" ~long_name:"debug" debug;

  let remaining_args = OptParser.parse_argv optparser in

  if List.is_empty remaining_args && not (Opt.is_set nb_nodes) then
    begin
      OptParser.usage optparser ();
      OptParser.error optparser "Missing input file."
    end
  else if not (List.is_empty remaining_args) then
    begin
      let filename = List.hd remaining_args in
      let basename = Filename.basename (Filename.remove_extension filename) in

      let graph = load_graph debug connect_subpatches resamplerDuration deadline filename in
      let graph = Option.default_delayed (fun () ->
      OptParser.usage optparser ();
      OptParser.error optparser "Wrong input format. Expecting: .pd ; .maxpat ; .ag";
      exit 1) graph in
      let graph = if Opt.get downsample then
          begin
            print_endline "Downsampling...";
            let durations node  =
              let label = G.V.label node in
              label.wcet |? 0.
            in
            if Opt.get exhaustive then
              begin
                ignore (Node_gen.load_possible_nodes (Opt.get node_file));
                run_exhaustive_downsampling graph basename output_dot output_audiograph report_graphs stats merge_resamplers
              end
            else
              begin
                Downsampling.downsample_components graph durations (Opt.get resamplerDuration) (Opt.get deadline);
                graph
              end
          end
        else graph
      in
      let output_name = if Opt.is_set output_name then Opt.get output_name else basename in
      if not (Opt.get exhaustive) then (*It was already outputed before*)
        begin
          if Opt.get output_dot then (print_endline "Outputing dot file.";output_graph output_name graph);
          if Opt.get output_audiograph then (print_endline "Outputing audiograph file.";Audiograph_export.export output_name graph);
        end;
    end
  else
    begin
      let nb_nodes = Option.default 4 (Opt.opt nb_nodes) in
      let edge_p = Opt.get edge_p in
      if Opt.get random then
        begin
          (*Random.self_init ();*)
          Random.init 36; (*if we need reproducible randomness... *)
          Printf.printf "Generating random graphs with %d nodes and edge probability %3f\n" nb_nodes edge_p
        end
      else if Opt.get use_graphs then
        begin
          Printf.printf "Using all graph in current directory: %s " (Sys.getcwd ())
        end
      else
        begin
          Printf.printf "Enumerating all connected directed graphs with %d nodes\n" nb_nodes;
          if nb_nodes > 6 then Printf.printf "High number of nodes... it is probably going to take ages!\n"
        end ;
      let open Node_gen in
      Printf.printf "Loading possible nodes\n";
      let nodes = load_possible_nodes (Opt.get node_file) in
      Printf.printf "Generating graphs...\n";
      let graphs = if Opt.get random then
            Random_graph.gen_random_dags nb_nodes edge_p (Opt.get nb_samples)
        else if Opt.get use_graphs then
          begin
            let files = Sys.readdir "." in
            let graphs = Array.filter_map
                (fun file ->
                   try load_graph debug connect_subpatches resamplerDuration deadline file
                with _ -> Printf.printf "\tFailed loading %s\n" file;None) files in
            Array.iter Flowgraph.coherent_iolets graphs;
            let graphs = Array.to_list (Array.map Random_graph.max_component graphs) in
            List.iter Flowgraph.coherent_iolets graphs;
            (*Keep only graphs with more than nb_nodes *)
            let graphs = List.filter (fun g -> G.nb_vertex g >= nb_nodes) graphs in
            (*Keep only graph with no cycles*)
            List.filter (fun g -> not (Flowgraph.TraverseDfs.has_cycle g)) graphs
          end
        else
          let open Enumeration in List.map  graph_to_flowgraph (gen_connected_directed_graphs nb_nodes)
      in
      Printf.printf " generated %d graphs\n" (List.length graphs);
      Printf.printf "Choosing nodes...";

      let graphs = List.map (fun g -> gen_possible_graph nodes g) graphs in
      Printf.printf " %d graphs in total\n" (List.length graphs);
      if Opt.get stats then
        begin
          let open Statistics in
          let statistics = avg_stats (List.map compute_stats graphs) in
          Printf.printf "Statistics: %s\n" (show_stats statistics);
        end;
      let basename = (if Opt.get random then "rand-" else "full-") ^(string_of_int nb_nodes)^"-node-graph-" in
      if Opt.get debug && Opt.get output_dot then (print_endline "Outputing dot files."; List.iteri (fun i g -> output_graph (basename ^ (string_of_int i)) g) graphs);
      if Opt.get debug && Opt.get output_audiograph then (print_endline "Outputing audiograph file.";List.iteri (fun i g -> Audiograph_export.export (basename ^ (string_of_int i)) g) graphs);
      if Opt.get exhaustive && Opt.get downsample then
        begin
        Printf.printf "Generating downsampling versions for each graphs\n";
        List.iteri (fun i graph ->
            if true || Opt.get debug then
              begin
                Printf.printf "Processing graph %d with %d nodes and %d edges.\n" i (Flowgraph.G.nb_vertex graph) (Flowgraph.G.nb_edges graph);
              end;
            ignore (run_exhaustive_downsampling graph (basename ^ (string_of_int i)) output_dot output_audiograph report_graphs stats merge_resamplers)) graphs
        end
    end;
  print_endline "Processing finished"

let () = main()
