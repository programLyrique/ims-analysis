(** Randomly generates DAGs *)
open Batteries
open Flowgraph

(* See https://stackoverflow.com/questions/12790337/generating-a-random-dag *)
let gen_dag nb_vertices nb_edges =
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
