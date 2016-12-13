(** A maxpat file is a json file. *)

open Batteries
open Printf
open Graph
open Flowgraph
open Max_parser
open Pd_lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  let state = ref Pd_lexer.Commands in
  try Pd_parser.prog (Pd_lexer.read state) lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    []
  | Pd_parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let main() =
  let filename = Sys.argv.(1) in

  if String.ends_with filename ".maxpat" then
    begin
    let graph = Max_parser.parse_maxpat filename in
    let file = Pervasives.open_out_bin (filename ^".dot") in
    Dot.output_graph file graph
    end
  else if String.ends_with filename ".pd" then
    begin
      let f = File.open_in filename in
        Pd_lexer.channel_to_tokens f;
      let f = File.open_in filename in
      let lexbuf = Lexing.from_channel f  in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      print_endline (Puredata.show_patch (parse_with_error lexbuf))
    end
  else
    begin
      print_endline "Wrong format";
      exit 1;
    end;
print_endline "Processing finished"

let () = main()
