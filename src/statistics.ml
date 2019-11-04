(** Computes various statistics about a flowgraph *)

open Flowgraph
open Graph
open Batteries

(** Can be used to store average values or max values. (that's why we use floating points)*)
type stats = {
  in_degree: float;
  max_in_degree: float;
  out_degree: float;
  max_out_degree: float;
  nb_edges: float;
  max_nb_edges: float;
  nb_vertices: float;
  max_nb_vertices: float;
  width: float;
  diameter:float;
  max_diameter: float;
} [@@deriving show]

(** Computes an average depending on each vertex *)
let avg_vertex f  graph =
  (G.fold_vertex f graph 0.) /. float_of_int (G.nb_vertex graph)

(** Average degrees *)
let avg_degrees graph =
  let in_d, out_d = G.fold_vertex (fun v (id, od) ->
      (id +. float_of_int (G.in_degree graph v), od +. float_of_int (G.out_degree graph v))) graph (0.,0.) in
  let nb_vertices = float_of_int (G.nb_vertex graph) in
  let in_degree = in_d /. nb_vertices in
  let out_degree = out_d /. nb_vertices in
  (in_degree, out_degree)

(**Maximum degrees *)
let max_degrees graph =
  let max_in_degree, max_out_degree = G.fold_vertex (fun v (id, od) ->
      (max id  (float_of_int (G.in_degree graph v)), max od  (float_of_int (G.out_degree graph v)))) graph (0.,0.) in
  (max_in_degree, max_out_degree)

module Weight =
struct
  type edge = G.E.t
  type t = int
  let weight edge = 1
  let compare = Pervasives.compare
  let add = (+)
  let sub = (-)
  let zero = 0
end
module Diameter =
struct
  include Path.Johnson(G)(Weight)
  (** Computes the diameter of a graph using Johnson shortest paths algorithm *)
  let diameter graph =
    let hashbtl = all_pairs_shortest_paths graph in
    HVV.fold (fun key value ma -> max value ma) hashbtl 0
end

(** Maximal antichain (or width )
    https://mathoverflow.net/questions/189161/fastest-algorithm-to-compute-the-width-of-a-poset
    How to compute it?
    1. Create a bipartite graph from the original graph. Forall x in G, create x' and x'' in B.
      Create an edge x'y'' iff x -> y
    2. Fold_fulkerson labbeling algorithm on B.
    3. For each chain in the partition
*)
let width graph = failwith "Not implemented!!"

let compute_stats graph =
  let in_degree,out_degree = avg_degrees graph in
  let max_in_degree, max_out_degree = max_degrees graph in
  let diameter = float_of_int (Diameter.diameter graph) in
  let nb_edges = float_of_int (G.nb_edges graph) in
  let nb_vertices = float_of_int (G.nb_vertex graph) in
  {in_degree; out_degree; max_in_degree; max_out_degree; diameter;nb_edges; nb_vertices;
   width=0.;max_nb_edges=0.;max_nb_vertices=0.; max_diameter=0.}


(**Computes the average of stats of a list of graphs *)
let avg_stats stats =
  let avg_ext f  = List.favg (List.map f stats) in
  let sum_ext f = List.fsum (List.map f stats) in
  let max_ext f = List.max (List.map f stats) in
  let diameter = avg_ext (fun s -> s.diameter)  in
  let max_diameter = max_ext (fun s -> s.diameter) in
  let sum_vertices = sum_ext (fun s -> s.nb_vertices) in
  let in_degree = (sum_ext (fun s -> s.nb_vertices *. s.in_degree)) /. sum_vertices  in
  let out_degree = (sum_ext (fun s -> s.nb_vertices *. s.out_degree)) /. sum_vertices in
  let max_in_degree = avg_ext (fun s -> s.max_in_degree)  in
  let max_out_degree = avg_ext (fun s -> s.max_out_degree) in
  let nb_edges = avg_ext (fun s -> s.nb_edges) in
  let max_nb_edges = max_ext (fun s -> s.nb_edges) in
  let max_nb_vertices = max_ext (fun s -> s.nb_vertices) in
  let nb_vertices = avg_ext (fun s -> s.nb_vertices) in
  {diameter;in_degree;out_degree;max_in_degree;max_out_degree;nb_edges;nb_vertices;
   max_nb_vertices; max_nb_edges; width=0.; max_diameter}
