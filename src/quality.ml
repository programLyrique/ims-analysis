(* Compute the quality of the graph (and its cost) *)
open Batteries
open Graph
open Flowgraph

module FlowgraphBfs = Traverse.Bfs(Flowgraph.G)

(** Computes the quality and cost (for instance temporal cost). Quality and costs are computed for each node with a user-set function.
    Issue: this version does stack-overflow *)
let compute_quality_cost2 graph preprocess quality cost =
  let sinks = G.fold_vertex (fun v l -> if G.out_degree graph v = 0 then v::l else l) graph [] in
  (*Update markings *)
  preprocess graph;
  let hashtbl = Hashtbl.create (G.nb_vertex graph) in
  let rec compute v =
    let qs = Hashtbl.find_option hashtbl (G.V.label v) in
    (*Printf.printf "className: %s\n" (G.V.label v).className;*)
    Option.default_delayed (fun () ->
          let qs = G.fold_pred (fun vertex l -> (compute vertex)::l) graph v [] in
          let qualities, costs = List.split qs in
          (*For critical path in the case of a multithreaded audio graph, we would have computed the one with maximum cost: List.max costs + cost v.
            Here we suppose we are on a single thread so the execution time of the graph is the sum of the execution of all the nodes.
          *)
          let va = (quality v qualities, List.fold_left (+.) 0. costs +. (cost v)) in
          Hashtbl.add hashtbl (G.V.label v) va;
          va
      )
      qs
  in
  let qu_co = List.map compute sinks in
  (*It is as if there were a special final node taking all the sinks as inputs *)
  let qualities, costs = List.split qu_co in
  (List.min qualities, List.max costs)

let compute_quality_cost graph preprocess quality cost =
  (*Update markings*)
  preprocess graph;
  let hashtbl = Hashtbl.create (G.nb_vertex graph) in
  let i = ref 1 in
  FlowgraphBfs.iter (fun v ->
      Printf.printf "%d " !i; incr i;
      Printf.printf "%s\n" (show_node (G.V.label v));
      (*Per the Bfs traversal, we should always have an associated value in the hashtbl *)
      let preds = G.fold_pred (fun vertex l -> (
            Printf.printf "\tpred: %s\n" (show_node (G.V.label vertex));
            Hashtbl.find hashtbl (G.V.label vertex))::l) graph v [] in

      (*Source node *)
      let q_c = if List.is_empty preds then
        begin
          (quality v [], cost v)
        end
      else
        begin
          let qualities, costs = List.split preds in
          (quality v qualities, List.fold_left (+.) 0. costs +. (cost v))
        end
      in
      Hashtbl.add hashtbl (G.V.label v) q_c
    ) graph;
  let sinks = G.fold_vertex (fun v l -> if G.out_degree graph v = 0 then v::l else l) graph [] in
  let qu_co = List.map (fun v -> Hashtbl.find hashtbl (G.V.label v)) sinks in
  (*It is as if there were a special final node taking all the sinks as inputs *)
  let qualities, costs = List.split qu_co in
  (List.min qualities, List.max costs)


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
