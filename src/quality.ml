(* Compute the quality of the graph (and its cost) *)
open Batteries
open Graph
open Flowgraph



(** Computes the quality and cost (for instance temporal cost). Quality and costs are computed for each node with a user-set function *)
let compute_quality_cost graph quality cost =
  let sinks = G.fold_vertex (fun v l -> if G.out_degree graph v = 0 then v::l else l) graph [] in
  let rec compute v =
    let qs = G.fold_pred (fun v l -> (compute v)::l) graph v [] in
    let qualities, costs = List.split qs in
    (*We compute the critical path (the one with maximum cost)*)
    (quality v qualities, List.max costs + (cost v))
  in
  let qu_co = List.map (fun sink -> (compute sink )) sinks in
  (*It ias as if there were a special final node taking all the sinks as inputs *)
  let qualities, costs = List.split qu_co in
  (List.min qualities, List.max costs)
