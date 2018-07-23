(* Generate a version of the audio graph with resamplers inserted to downsample some subpaths of the graph *)
open Graph


module Edge = struct
  type t = int*float ref *int (* Input port, resmapling factor, output port *)
  let compare = Pervasives.compare
  let equal = (=)
  let default = (0,ref 1.,0)
  let resample (i, r, o) r' = r := r'
  let resamplingratio (i, r, o) = !r
end

module Node = struct
  type t = Flowgraph.node * bool ref (* Bool indicates if there is at least one input that has a changed sampling ratio for this node*)
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let empty = (Flowgraph.({id=""; nb_inlets=0; nb_outlets=0; className=""; text=None ; more=[] }), false)
  let is_valid n = Flowgraph.(not (n.id = "" || n.className = ""))
  let is_on_resampled_path ((node, b) : t) = !b
  let on_resampled_path ((node, b) : t) = b := true
end


module G = struct
  include Imperative.Digraph.ConcreteBidirectionalLabeled(Node)(Edge) (* Bidirectional because we do backtracking *)
  let empty  () = create ()
  let add_edge_e t edge = add_edge_e t edge; t
end
module Mapper = Gmap.Edge(Flowgraph.G)(G)
module Scheduler = Topological.Make(G)

let get_schedule graph =
  Array.of_list (List.rev (Scheduler.fold (fun node l -> node::l) graph []))

(* Choose to degrade everything after node i *)
let exhaustive_heuristic graph durations first_node_to_degrade =
  let schedule = get_schedule graph in
  let nb_tasks = Array.length schedule  in
  let cumulative_durations = Array.create_float nb_tasks  in
  assert (0 <= first_node_to_degrade && first_node_to_degrade <= nb_tasks - 1 );
  for i = first_node_to_degrade to nb_tasks - 1 do
    (* Here without path merging *)

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




(* graph is the audio graph
   durations : node -> float is the WCET/AWT of each computation node in s
   resamplerDuration : factor -> float is the time in s to resample by the factor
   budget is the available time budget for the whole graph
*)
let dowsample_components graph durations resamplerDuration budget =
  let ratio_graph = Mapper.map (fun (i,(pi, po), o) -> (i, (pi, ref 1., po), o)) graph in
  ()
  (* We need to keep track of *)
