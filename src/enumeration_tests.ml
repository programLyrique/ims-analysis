(* Tests for enumeration.ml  *)
open OUnit2
open Batteries


let enumerate_degraded_versions test_ctxt =
  let open Flowgraph in
  let node1 = Flowgraph.G.V.create (Node.make "id-1" 1 1 "plop")  in
  let node2 = Flowgraph.G.V.create (Node.make "id-2" 1 1 "plop") in
  let node3 = Flowgraph.G.V.create (Node.make "id-3" 2 1 "mix") in
  let edge1 = Flowgraph.G.E.create node1 (1,1) node3 in
  let edge2 = Flowgraph.G.E.create node2 (1,2) node3 in
  let graph = Flowgraph.G.create ~size:3 () in
  let res = Enumeration.enumerate_degraded_versions graph in
  assert_equal (List.length res) 0


let suite = "enumeration" >::: ["enumerate_degraded_versions" >:: enumerate_degraded_versions]
