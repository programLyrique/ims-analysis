(* Tests for enumeration.ml  *)
open OUnit2
open Batteries
open Flowgraph


let enumerate_degraded_versions test_ctxt =
  let open Enumeration in
  let node1 = G.V.create (Node.make "id-1" 1 1 "plop")  in
  let node2 = G.V.create (Node.make "id-2" 1 1 "plop") in
  let node3 = G.V.create (Node.make "id-3" 2 1 "mix") in
  let node4 = G.V.create (Node.make "id-4" 1 1 "reverb") in
  let node5 = G.V.create (Node.make "id-5" 1 1 "out") in
  let edge1 = G.E.create node1 (1,1) node3 in
  let edge2 = G.E.create node2 (1,2) node3 in
  let edge3 = G.E.create node3 (1,1) node4 in
  let edge4 = G.E.create node4 (1,1) node5 in
  let graph = G.create ~size:5 () in
  G.add_edge_e graph edge1;
  G.add_edge_e graph edge2;
  G.add_edge_e graph edge3;
  G.add_edge_e graph edge4;
  let res = Enumeration.enumerate_degraded_versions_vertex (graphflow_to_graph graph) in
  assert_equal 4 ~printer:string_of_int (List.length res)

let test_plop test_ctxt =
  let open Enumeration in
  let node1 = G.V.create (Node.make "id-1" 1 1 "plop")  in
  let node2 = G.V.create (Node.make "id-2" 1 1 "plop") in
  let node3 = G.V.create (Node.make "id-3" 2 1 "mix") in
  let node4 = G.V.create (Node.make "id-4" 1 1 "out") in
  let edge1 = G.E.create node1 (1,1) node3 in
  let edge2 = G.E.create node2 (1,2) node3 in
  let edge3 = G.E.create node3 (1,1) node4 in
  let graph = G.create ~size:4 () in
  G.add_edge_e graph edge1;
  G.add_edge_e graph edge2;
  G.add_edge_e graph edge3;
  let g2 = G.copy graph in
  assert (G.mem_vertex g2 node1)

let enumerate_connected_graphs test_ctxt =
  let open Enumeration in
  let graphs = gen_connected_directed_graphs 5 in
  (*List.iter (fun g -> Printf.printf "%s\n" (Enumeration.G.format_graph g)) graphs;*)
  assert_equal 838 ~printer:string_of_int (List.length graphs)

let enumerate_simple_graph test_ctxt =
  let open Enumeration in
  let graph, _,_ = Audiograph_parser_tests.parse_file "tests/simple_graph.ag" in
  let res = Enumeration.enumerate_degraded_versions_vertex (flowgraph_to_graphflow graph) in
  assert_equal 4 ~printer:string_of_int (List.length res)

let enumerate_full_graph test_ctxt =
  let open Enumeration in
  let graph, _,_ = Audiograph_parser_tests.parse_file "tests/full-5-node-graph-7.ag" in
  let res = Enumeration.enumerate_degraded_versions_vertex (flowgraph_to_graphflow graph) in
  assert_equal 2 ~printer:string_of_int (List.length res)


let suite = "enumeration" >::: ["enumerate_degraded_versions" >:: enumerate_degraded_versions;
                                "test_plop" >:: test_plop;
                                "enumerate_connected_graphs" >:: enumerate_connected_graphs;
                                "enumerate_simple_graph" >:: enumerate_simple_graph;
                                "enumerate_full_graph" >:: enumerate_full_graph]
