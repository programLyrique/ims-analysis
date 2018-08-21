(*testing auiograph_lexer and audiograph_parser *)
open OUnit2
open Batteries
open Lexing


let parse_file filename =
  if String.ends_with filename ".ag" then
      begin
        let f = File.open_in filename in
        let lexbuf = Lexing.from_channel f  in
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
        let nodes,edges = Audiograph_lexer.parse_with_error_ag lexbuf in
        Flowgraph.build_graph nodes edges
      end
  else
    Flowgraph.G.create ~size:0 ()

let parsing test_ctxt =
  let open Flowgraph in
  let graph = parse_file "audiograph_test.ag" in
  let target_graph = G.create ~size:2 () in
  let node1 = G.V.create  {id="id-1"; nb_inlets=1; nb_outlets=2; className="plop"; text=Some "salut" ; wcet=Some 0.; more=[] } in
  let node2 = G.V.create (Node.make "id-1" 1 0 "plop2") in
  let edge =  G.E.create node1 (2,1) node2 in
  G.add_edge_e target_graph edge;
  assert_equal ~printer:G.format_graph ~cmp:equal_content target_graph graph


let suite = "audiograph_parser" >::: ["parsing" >:: parsing]
