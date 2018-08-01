(* Generate a version of the audio graph with resamplers inserted to downsample some subpaths of the graph *)
open Graph


module Edge = struct
  type t = int*float ref *int (* Input port, resmapling factor, output port *)
  let compare = Pervasives.compare
  let equal = (=)
  let default = (0,ref 1.,0)
  let resample (i, r, o) r' = r := r'
  let resamplingratio (i, r, o) = !r
  let to_flowgraph_label (i, r, o) = (i, o)
end

module Node = struct
  type t = Flowgraph.G.V.t * bool ref (* Bool indicates if there is at least one input that has a changed sampling ratio for this node*)
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let empty = (Flowgraph.({id=""; nb_inlets=0; nb_outlets=0; className=""; text=None ; more=[] }), false)
  let is_valid n = Flowgraph.(not (n.id = "" || n.className = ""))
  let is_on_resampled_path ((node, b) : t) = !b
  let on_resampled_path ((node, b) : t) = b := true
  let to_flowgraph_node ((node, b) : t) =  node
end


module G = struct
  include Imperative.Digraph.ConcreteBidirectionalLabeled(Node)(Edge) (* Bidirectional because we do backtracking *)
  let empty  () = create ()
  let add_edge_e t edge = add_edge_e t edge; t
  let to_flowgraph_edge (i, l, o) = Flowgraph.G.E.create (Node.to_flowgraph_node i) (Edge.to_flowgraph_label l) (Node.to_flowgraph_node o)
end
module Mapper = Gmap.Edge(Flowgraph.G)(G)
module Scheduler = Topological.Make(G)

let get_schedule graph =
  Array.of_list (List.rev (Scheduler.fold (fun node l -> node::l) graph []))

(* Choose to degrade everything after node i *)
let exhaustive_heuristic graph schedule first_node_to_degrade =
  let nb_tasks = Array.length schedule  in
  assert (0 <= first_node_to_degrade && first_node_to_degrade <= nb_tasks - 1 );
  for i = first_node_to_degrade to nb_tasks - 1 do
    (* Here without path merging: we will do it as a later stage *)

    let current_node = schedule.(i) in
    (* if there is one input of the node which has been degraded at least?
       In that case, we check that all the inputs are resampled and we resample the ones that are not in case.
    No need to resample the outputs. *)
    if Node.is_on_resampled_path current_node then
      begin
        G.iter_pred_e (fun edge -> if Node.is_on_resampled_path (G.E.src edge) then
                          Edge.resample (G.E.label  edge) 0.5) graph current_node;
        (* All successors are on a resampled path now*)
        G.iter_succ_e (fun edge ->  Node.on_resampled_path (G.E.dst edge)) graph current_node
      end
    else
      (* Here we could just insert a resampling node to which all the inputs converge instead of inserting one per output*)
      G.iter_succ_e (fun edge -> Edge.resample (G.E.label edge) 0.5; Node.on_resampled_path (G.E.dst edge)) graph current_node
  done;
  graph

let unique_id =
  let id  = ref 0 in
    function  () -> incr id; !id

module Traversal = Traverse.Dfs(G)
(* Take the ratio graph previously computed and modifies graph accordingly.
*)
let ratio_graph_to_graph ratio_graph graph =
  (* Insert a resampler on an edge if needed *)
  let insert_resampler edge  =
    let (i, label, o) = edge in
    let ratio = Edge.resamplingratio label in
    if ratio != 1.0 then
      (* We need to modify the topology of the graph. *)
      Flowgraph.G.remove_edge_e graph (G.to_flowgraph_edge edge);
    let resampler_node = Flowgraph.(G.V.create {id="res" ^(string_of_int (unique_id ())); nb_inlets=1; nb_outlets=1; className="resampler"; text=None ; more=[("ratio", string_of_float ratio)] }) in
    let (pi, _, po) = label in
    (* Here, i and o really store original vertices from the original graph so we are not creating fresh vertices*)
    let e1 = Flowgraph.G.E.create (Node.to_flowgraph_node i) (pi, 1) resampler_node in
    let e2 = Flowgraph.G.E.create resampler_node (1, po) (Node.to_flowgraph_node o) in
    Flowgraph.G.add_edge_e graph e1;
    Flowgraph.G.add_edge_e graph e2
  in
  Traversal.prefix (G.iter_succ_e insert_resampler ratio_graph) ratio_graph


(* Can merge common channels with sample resampling ratio but inserting new channels and only one resampler.
       If merging incoming:
        - R --\              --\
        - R -- e1    becomes -- R -- e1
        - R --/              --/
   This pass should be done one the ratio graph or on the flowgraph?...
   Will be more efficient on ratio graph when merging incoming, because access to predecessors is O(1).
*)
let merge_resamplers graph =  (*TODO *)
  let open Flowgraph in
  let merge_resamplers vertex =
    (* Merge incoming. Here we suppose that we use only two resampling ratio.
       TODO: just differentiate between 0.5 and 2
    *)
    let to_merge v l = let lbl = G.V.label v in if Flowgraph.(lbl.className = "resampler") then v::l else l in
    let incoming_to_merge  = G.fold_pred to_merge graph vertex [] in
    let outcoming_to_merge = G.fold_succ to_merge graph vertex [] in

    let incoming_length = List.length incoming_to_merge in
    if incoming_length > 0 then
      begin
        let first_resampler = G.V.label (List.hd incoming_to_merge) in
        let incoming_resampler = G.V.create {id="res" ^(string_of_int (unique_id ()));
                                             nb_inlets=incoming_length; nb_outlets=1; className="resampler";
                                             text=None ; more=[("ratio", List.assoc "ratio" first_resampler.more)] } in
        G.add_vertex graph incoming_resampler;
        List.iteri (fun i v ->
            let incoming_edge = G.pred_e graph v in
            assert(1 = List.length incoming_edge );
            let src = G.E.src (List.hd incoming_edge) in
            G.remove_vertex graph v;
            let new_edge = G.E.create src (1, i) incoming_resampler in
            G.add_edge_e graph new_edge
          ) incoming_to_merge;
        let outcoming_edge = Flowgraph.G.E.create incoming_resampler  (1,1) vertex in
        G.add_edge_e graph outcoming_edge
      end;
    (* Should aim at factorizing that *)
    let outcoming_length = List.length outcoming_to_merge in
    if outcoming_length > 0 then
      begin
        let first_resampler = G.V.label (List.hd outcoming_to_merge) in
        let outcoming_resampler = G.V.create {id="res" ^(string_of_int (unique_id ()));
                                             nb_inlets=1; nb_outlets=outcoming_length; className="resampler";
                                             text=None ; more=[("ratio", List.assoc "ratio" first_resampler.more)] } in
        G.add_vertex graph outcoming_resampler;
        List.iteri (fun i v ->
            let outcoming_edge = G.succ_e graph v in
            assert(1 = List.length outcoming_edge );
            let dst = G.E.dst (List.hd outcoming_edge) in
            G.remove_vertex graph v;
            let new_edge = G.E.create outcoming_resampler (i, 1) outcoming_resampler in
            G.add_edge_e graph new_edge
          ) outcoming_to_merge;
        let incoming_edge = Flowgraph.G.E.create vertex  (1,1) outcoming_resampler in
        G.add_edge_e graph incoming_edge
      end
in
let module Traversal = Traverse.Dfs(G) in
Traversal.prefix merge_resamplers graph



(* graph is the audio graph
   durations : node -> float is the WCET/AET of each computation node in s
   resamplerDuration : factor -> float is the time in s to resample by the factor
   budget is the available time budget for the whole graph
*)
let dowsample_components graph durations resamplerDuration budget =
  let ratio_graph = Mapper.map (fun edge -> let (pi, po) = Flowgraph.G.E.label edge in
      ((Flowgraph.G.E.src edge, ref false), (pi, ref 1., po), (Flowgraph.G.E.dst edge, ref false)) ) graph in
  let schedule = get_schedule ratio_graph in
  let nb_tasks = Array.length schedule in
  let remaining_duration = Array.fold_left (fun sum (node,_) -> sum +. (durations node)) 0. schedule in
  (*let cumulative_durations = Array.create_float nb_tasks in
  cumulative_durations.(0) <- durations (fst schedule.(0));
  for i=1 to nb_tasks - 1 do
    cumulative_durations.(i) <- cumulative_durations.(i-1) +. durations (fst (schedule.(i)));
    done;*)
  if remaining_duration > budget then (* Problem, not enough time to execute everything*)
    (* Find where to degrade *)
    let rec find_where_to_degrade i durations_left durations_right =
      if i >= 0 then
        let current_node = fst schedule.(i) in
        let current_duration = durations current_node in
        let durations_left = durations_left -. current_duration in
        let durations_right = durations_right +. current_duration in
        (* Degrading right *)
        let total_duration = durations_left +. durations_right /. 2. in
        if total_duration <= budget then
          i
        else
          find_where_to_degrade (i - 1) durations_left durations_right
      else
        0
    in
    let node_to_degrade = find_where_to_degrade (nb_tasks - 1) remaining_duration 0. in
    ignore (exhaustive_heuristic ratio_graph schedule node_to_degrade);
    ratio_graph_to_graph ratio_graph graph
