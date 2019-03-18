(** Testing Puredata parsing *)

open OUnit2
open Batteries
open Lexing
open Printf
open Pd_parser
open Lexing
open Flowgraph

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error_pd lexbuf =
  let state = ref Pd_lexer.Commands in
  try Pd_parser.prog (Pd_lexer.read state) lexbuf with
  | Pd_lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit(-1)
  | Pd_parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let parse_puredata_patch filename =
  let f = File.open_in filename in
  let lexbuf = Lexing.from_channel f  in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let patch = parse_with_error_pd lexbuf in
  Puredata.build_graph patch

let parse_simple test_ctxt =
  let graph = parse_puredata_patch "tests/J09.bandlimited.pd" in
  assert_equal ~printer:string_of_int 63 (Flowgraph.G.nb_vertex graph);
  assert_equal ~printer:string_of_int 69 (Flowgraph.G.nb_edges graph)

let parse_with_subpatches test_ctxt =
  let graph = parse_puredata_patch "tests/I04.noisegate.pd" in
  assert_equal ~printer:string_of_int 114 (Flowgraph.G.nb_vertex graph);
  assert_equal ~printer:string_of_int 128 (Flowgraph.G.nb_edges graph)

let suite = "puredata_parser" >::: ["parse_simple" >:: parse_simple;
                                     "parse_with_subpatches" >:: parse_with_subpatches]
