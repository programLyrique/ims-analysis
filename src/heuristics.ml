(** Heuristics to degrade. Generate several degraded versions *)
open Graph
open Batteries
open Flowgraph
open Downsampling


(** Topological sort *)
let topo_degradation graph = 
  let ratio_graph = graph_to_ratio_graph graph in
  let schedule = get_schedule ratio_graph in
  let nb_tasks = Array.length schedule in

  (* Try to generate a degraded graph for every sucessive node  in the schedule *)
  Array.init nb_tasks (fun i -> exhaustive_heuristic (DeepCopy.copy ratio_graph) schedule i)