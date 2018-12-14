(* Tests for enumeration.ml  *)
open OUnit2
open Batteries


let enumerate_degraded_versions test_ctxt =
  let open Flowgraph in
  let node1 = Flowgraph.G.V.create (Node.make "id-1" 1 1 "plop")  in
  let node2 = Flowgraph.G.V.create (Node.make "id-2" 1 1 "plop") in
  let node3 = Flowgraph.G.V.create (Node.make "id-3" 2 1 "mix") in
  let node4 = Flowgraph.G.V.create (Node.make "id-4" 1 1 "out") in
  let edge1 = Flowgraph.G.E.create node1 (1,1) node3 in
  let edge2 = Flowgraph.G.E.create node2 (1,2) node3 in
  let edge3 = Flowgraph.G.E.create node3 (1,1) node4 in
  let graph = Flowgraph.G.create ~size:4 () in
  Flowgraph.G.add_edge_e graph edge1;
  Flowgraph.G.add_edge_e graph edge2;
  Flowgraph.G.add_edge_e graph edge3;
  let res = Enumeration.enumerate_degraded_versions graph in
  assert_equal ~printer:string_of_int (List.length res) 0


let suite = "enumeration" >::: ["enumerate_degraded_versions" >:: enumerate_degraded_versions]
