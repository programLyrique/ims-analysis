(* Tests for random_graph.ml  *)
open OUnit2
open Batteries
open Flowgraph


let random_graph_v_e test_ctxt =
  Random.init 678;
  let nb_vertices = 30 and nb_edges = 40 in
  let graph = Random_graph.gen_dag_v_e nb_vertices nb_edges in
  assert_equal  ~printer:string_of_int nb_vertices (G.nb_vertex graph);
  assert_equal  ~printer:string_of_int nb_edges (G.nb_edges graph)

let random_graph_v test_ctxt =
  Random.init 236;
  let nb_vertices = 60  in
  let p = 0.3 in
  let graph = Random_graph.gen_dag_v nb_vertices p in
  assert_equal  ~printer:string_of_int nb_vertices (G.nb_vertex graph)


  let suite = "random graph" >::: ["random_graph_v_e" >:: random_graph_v_e;
                                   "random_graph_v" >:: random_graph_v]
