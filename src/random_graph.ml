(** Randomly generates DAGs *)
open Batteries
open Flowgraph

(* See https://stackoverflow.com/questions/12790337/generating-a-random-dag
   Generate a Dag goven a number of edges and a number of vertices *)
let gen_dag_v_e nb_vertices nb_edges =
  let n = nb_vertices * (nb_vertices - 1) / 2 in
  assert (0 < nb_edges);
  assert (nb_edges < n);
  let sample = Array.init nb_edges (fun i -> Random.int (n - nb_edges + 1))  in
  Array.sort compare sample;
  (*Make unique *)
  Array.modifyi (fun i a -> a + i ) sample;
  assert (Array.for_all (fun a -> a >=0 && a <= n - 1) sample);

  let endpoints = Enum.range ~until:(nb_vertices - 1) 0 in
  let endpoints = Random.shuffle endpoints in

  let vertices = Array.init nb_vertices (fun i -> G.V.create (Node.make ("id-" ^ string_of_int i ) 1 1 "plop")) in
  let graph = G.create ~size:nb_vertices () in
  Array.iter (fun v -> G.add_vertex graph v) vertices;
  Array.iter (fun v ->
      let tail = int_of_float (0.5 +. sqrt((float_of_int v +. 1.) *. 2.)) in
      let head = v - tail * (tail - 1) / 2 in
      let src = endpoints.(tail) in
      let dst = endpoints.(head) in
      G.add_edge_e graph (G.E.create vertices.(src) (1,1) vertices.(dst))
    ) sample;
  graph

(* Generate a Dag given a number of nodes and a probability of having an edge between two vertices *)
let  gen_dag_v nb_vertices p =
  (*So that we don't have the same ids... Do we actually really care as node generation will be done later? *)
  let endpoints = Enum.range ~until:(nb_vertices - 1) 0 in
  let endpoints = Random.shuffle endpoints in
  (*Get the actual nodes*)
  let vertices = Array.init nb_vertices (fun i -> G.V.create (Node.make ("id-" ^ string_of_int endpoints.(i) ) 1 1 "plop")) in
  let graph = G.create ~size:nb_vertices () in
  Array.iter (fun v -> G.add_vertex graph v) vertices;
  let one = 1. +. epsilon_float in (*We want an uniform distribution in the close range [0,1]*)
  for i = 0 to nb_vertices - 1 do
    for j = 0 to i - 1 do
      if Random.float one <= p then
        G.add_edge_e graph (G.E.create vertices.(i) (1,1) vertices.(j))
    done
  done;
  graph
