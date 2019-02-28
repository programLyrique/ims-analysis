(* Compute the quality of the graph (and its cost) *)
open Batteries
open Graph
open Flowgraph



(** Computes the quality and cost (for instance temporal cost). Quality and costs are computed for each node with a user-set function *)
let compute_quality_cost graph preprocess quality cost =
  let sinks = G.fold_vertex (fun v l -> if G.out_degree graph v = 0 then v::l else l) graph [] in
  (*Update markings *)
  preprocess graph;
  (*TODO: memoization because otherwise we compute again*)
  let rec compute v =
    (*Printf.printf "className: %s\n" (G.V.label v).className;*)
    let qs = G.fold_pred (fun v l -> (compute v)::l) graph v [] in
    let qualities, costs = List.split qs in
    (*For critical path in the case of a multithreaded audio graph, we would have computed the one with maximum cost: List.max costs + cost v.
      Here we suppose we are on a single thread so the execution time of the graph is the sum of the execution of all the nodes.
    *)
    (quality v qualities, List.fold_left (+.) 0. costs +. (cost v))
  in
  let qu_co = List.map (fun sink -> (compute sink )) sinks in
  (*It is as if there were a special final node taking all the sinks as inputs *)
  let qualities, costs = List.split qu_co in
  (List.min qualities, List.max costs)


module FlowgraphBfs = Traverse.Bfs(Flowgraph.G)


(** We mark each node by its degraded status or not. 0 is not degraded, and 1 is. *)
(*Rather update successors?*)
let update_markings graph =
  G.Mark.clear graph;
  FlowgraphBfs.iter (fun vertex ->
      let preds = G.pred graph vertex in
      if not (List.is_empty preds) then
        begin
          (* We do not have necessarily only pred resamplers. But if at least one then...*)
          let fst_node = List.find_opt (fun v -> (G.V.label v).className = "resampler") preds in
          let new_mark =
            if Option.is_some fst_node then
              begin
                let fst_node = G.V.label (Option.get fst_node) in
                let ratio = Downsampling.get_ratio fst_node.more in
                (*Heuristic check for correction. There might be resamplers several edges before and not immediatly before. *)
                assert (List.for_all (fun v -> if (G.V.label v).className = "resampler" then ratio = Downsampling.get_ratio (G.V.label v).more else true ) preds);
                if ratio < 1. then 1 else if ratio > 1. then 0 else failwith "Weird resampler with ratio 1.0!!\n"
              end
            else
              begin
                let mark =  List.max (List.map G.Mark.get preds) in (*If there at least one degraded, then it is degraded. The other pred have not propagated yet.*)
                (*let predicate = List.for_all (fun v -> mark = G.Mark.get v) preds in
                if not predicate then
                  Printf.printf "Classname: %s\n" (G.V.label vertex).className;
                  assert predicate;*)
                mark
              end
          in
          G.Mark.set vertex new_mark
        end
    ) graph

(** Computes the quality of the node given the qualities coming into each input *)
let quality node preds =
  let lbl = G.V.label node in
  (*If it is an input, the quality is the best possible one. *)
  if List.is_empty preds then 1.
  else
    let agregate_prev = match lbl.className with
    | "mix" -> let sum = List.reduce (+.) preds in sum /. (float_of_int (List.length preds))
    | _ -> List.min preds
    in
    agregate_prev *. (if G.Mark.get node = 1 then 0.9  else 1.)


let cost node =
  let lbl = G.V.label node in
  let degraded = if G.Mark.get node = 1 then 0.5 else 1.0 in
  let c = Option.default_delayed (fun () ->
    match lbl.className with
    | "resampler" -> Option.default 0.9 (Node_gen.get_wcet_resampler ())
    | "mix" -> degraded *. Option.default_delayed (fun () -> 0.1 *. float_of_int lbl.nb_inlets +. 0.2 *. float_of_int lbl.nb_outlets) (Node_gen.get_wcet_mixer lbl.nb_inlets lbl.nb_outlets)
    | "in" | "out" -> 0.
    | classname -> degraded *. (Option.default 1. (Node_gen.get_wcet_by_name classname)) )
      lbl.wcet in
  (*Printf.printf "node: %s; class: %s; wcet: %f \n" lbl.id lbl.className c;*) c

let quality_cost graph =
  compute_quality_cost graph update_markings quality cost
