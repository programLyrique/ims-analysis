(* Generate a version of the audio graph with resamplers inserted to downsample some subpaths of the graph *)
open Graph
open Batteries
open BatLog



module Edge = struct
  type t = int*float ref *int (* Input port, resmapling factor, output port *)
  let compare (i1, r1, o1) (i2, r2, o2) = Pervasives.compare !r1 !r2
  let equal (i1, r1, o1) (i2, r2, o2) = i1 = i2 && !r1 = !r2 && o1 = o2
  let default = (0,ref 1.,0)
  let resample (i, r, o) r' = r := r'
  let resamplingratio (i, r, o) = !r
  let to_flowgraph_label (i, r, o) = (i, o)
end

module Node = struct
  type t = Flowgraph.G.V.t (* Bool indicates if there is at least one input that has a changed sampling ratio for this node*)
  let compare = Flowgraph.Node.compare
  let hash n = Hashtbl.hash Flowgraph.((G.V.label n).id) (*We actually don't care about the bool. And the ref would create problems with the hash function anyway *)
  let equal n1 n2 = Flowgraph.Node.equal n1 n2
  let empty : t = Flowgraph.G.V.create Flowgraph.Node.empty
  let is_valid n = Flowgraph.(not (n.id = "" || n.className = ""))
  let is_on_resampled_path tbl node = Hashtbl.find_default tbl node false
  let on_resampled_path tbl node = Hashtbl.replace tbl node true
  let to_flowgraph_node node =  node
end


let make_resampler_node id nb_inlets ratio =
  let open Flowgraph in
   {id; nb_inlets; nb_outlets=1; className="resampler"; text=None ; wcet=Some 0.; more=[("ratio", string_of_float ratio)] }

module G = struct
  include Imperative.Digraph.ConcreteBidirectionalLabeled(Node)(Edge) (* Bidirectional because we do backtracking *)
  let empty  () = create ()
  let add_edge_e t edge = add_edge_e t edge; t
  let to_flowgraph_edge (i, l, o) = Flowgraph.G.E.create (Node.to_flowgraph_node i) (Edge.to_flowgraph_label l) (Node.to_flowgraph_node o)
  (*Not the most efficient as we don't stop if it starts to differ*)
  let equal t1 t2 =  fold_edges_e (fun edge b -> b && mem_edge_e t2 edge ) t1 true && fold_edges_e (fun edge b -> b && mem_edge_e t1 edge ) t2 true

end
module Mapper = Gmap.Edge(Flowgraph.G)(G)
module Scheduler = Topological.Make(G)

(* Module needed because of the references in edge declarations. So G.copy would not deep copy. *)
module DeepCopy  = struct
  include Gmap.Edge(G)(G)
  let copy = map (fun (n1, lbl, n2) ->
      let (p1,res_fact, p2)  = lbl in
      G.E.create n1  (p1, ref !res_fact, p2) n2
    )
end

let format_edge  ((n1, (i, r, o), n2) : G.E.t) = Printf.sprintf "(%s, (%d, %f, %d), %s)\n"
    (Flowgraph.show_node (Flowgraph.G.V.label n1))
     i !r o
    (Flowgraph.show_node (Flowgraph.G.V.label n2))
let format_graph graph = G.fold_edges_e (fun edge s -> Printf.sprintf "%s%s" s (format_edge edge)) graph ""

let get_schedule graph =
  Array.of_list (List.rev (Scheduler.fold (fun node l -> node::l) graph []))

(* Choose to degrade everything after node i *)
let exhaustive_heuristic graph schedule first_node_to_degrade =
  let nb_tasks = Array.length schedule  in
  let hashtbl = Hashtbl.create nb_tasks in
  assert (0 <= first_node_to_degrade && first_node_to_degrade <= nb_tasks - 1 );
  (*Printf.printf "There are %d tasks\n" nb_tasks;*)
  for i = first_node_to_degrade to nb_tasks - 1 do
    (* Here without path merging: we will do it as a later stage *)

    let current_node = schedule.(i) in

    (* if there is one input of the node which has been degraded at least?
       In that case, we check that all the inputs are resampled and we resample the ones that are not in case.
       No need to resample the outputs. *)
    (*Printf.printf ` "#\t########\nCurrent node %d : %s\n" i (Flowgraph.show_node (Flowgraph.G.V.label (Node.to_flowgraph_node current_node)));
    Printf.printf "$$ Graph is currently: %s" (format_graph graph);
      Hashtbl.print ~first:"$$ Hastbl is: " ~last:"\n" (fun out k -> BatInnerIO.write_string out Flowgraph.(show_node (G.V.label k))) (fun out v -> Printf.fprintf out "%b" v) stdout hashtbl;*)
    (*Printf.printf "Current node : %s\n" (dump current_node);
      Printf.printf "graph is currently: %s \n" (dump graph);*)
    if Node.is_on_resampled_path hashtbl current_node then
      begin
        (* If an incoming node was not on a resampling path, we need to resample the edge between it and the current node*)
        G.iter_pred_e (fun edge -> if not (Node.is_on_resampled_path hashtbl (G.E.src edge)) then
                          Edge.resample (G.E.label edge) 0.5) graph current_node;
        (* All successors are on a resampled path now*)
        G.iter_succ_e (fun edge ->  Node.on_resampled_path hashtbl (G.E.dst edge)) graph current_node
      end
    else
      begin
        (* Here we could just insert a resampling node to which all the inputs converge instead of inserting one per output. Instead, we have a merging phase later on*)
        G.iter_succ_e (fun edge -> Edge.resample (G.E.label edge) 0.5; Node.on_resampled_path hashtbl (G.E.dst edge)) graph current_node
      end;
    (*Printf.printf "$$ Modified graph is currently: %s \n" (format_graph graph);*)
  done

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
      begin
        (*let (i, r, o) = edge in*)
        (* We need to modify the topology of the graph. *)
        let edge = G.to_flowgraph_edge edge in
        (*print_endline (Flowgraph.G.format_graph graph);
        print_endline (Flowgraph.G.format_edge edge);

        let v1 = Node.to_flowgraph_node i and v2 = Node.to_flowgraph_node o in
        Flowgraph.G.iter_vertex (fun v -> print_endline ( dump v)) graph;*)
        Flowgraph.G.remove_edge_e graph edge;
        let resampler_node = Flowgraph.(G.V.create {id="res" ^(string_of_int (unique_id ())); nb_inlets=1; nb_outlets=1; className="resampler"; text=None ; wcet=Some 0.; more=[("ratio", string_of_float ratio)] }) in
        let (pi, _, po) = label in
        (* Here, i and o really store original vertices from the original graph so we are not creating fresh vertices*)
        let e1 = Flowgraph.G.E.create (Node.to_flowgraph_node i) (pi, 1) resampler_node in
        let e2 = Flowgraph.G.E.create resampler_node (1, po) (Node.to_flowgraph_node o) in
        Flowgraph.G.add_edge_e graph e1;
        Flowgraph.G.add_edge_e graph e2
      end
  in
  Traversal.prefix (G.iter_succ_e insert_resampler ratio_graph) ratio_graph


(* Can merge common channels with sample resampling ratio but inserting new channels and only one resampler.
       If merging incoming:
        - R --\              --\
        - R -- mixer -- e1    becomes -- R -- e1
        - R --/              --/
   Or:
          /-- R - e1                   /-- e1
       e.p -- R - e2         e.p -- R - -- e3
          \-- R - e3                   \-- e3

   This pass should be done one the ratio graph or on the flowgraph?...
   Will be more efficient on ratio graph when merging incoming, because access to predecessors is O(1).
*)
let merge_resamplers graph =
  let open Flowgraph in
  let vertices_to_remove = Enum.empty () in
  let merge_resamplers_before vertex =
    (* Merge incoming. Here we suppose that we use only two resampling ratio.
       TODO: just differentiate between 0.5 and 2
    *)
    (*We remove the mixer *)
    (* If the current node is a mixer, we can merge incoming nodes *)
    let to_merge v l = let lbl = G.V.label v in if Flowgraph.(lbl.className = "resampler") then v::l else l in

    if Flowgraph.((G.V.label vertex).className = "mixer") then
      begin
        let incoming_to_merge  = G.fold_pred to_merge graph vertex [] in
        let incoming_length = List.length incoming_to_merge in
        if incoming_length > 0 then
          begin
            let first_resampler = G.V.label (List.hd incoming_to_merge) in
            let incoming_resampler = G.V.create {id="res" ^(string_of_int (unique_id ()));
                                                 nb_inlets=incoming_length; nb_outlets=1; className="resampler";
                                                 wcet=Some 0.;
                                                 text=None ; more=[("ratio", List.assoc "ratio" first_resampler.more)] } in
            G.add_vertex graph incoming_resampler;
            List.iteri (fun i v ->
                let incoming_edge = G.pred_e graph v in
                assert(1 = List.length incoming_edge );
                let src = G.E.src (List.hd incoming_edge) in
                G.remove_vertex graph v;
                let new_edge = G.E.create src (1, i+1) incoming_resampler in
                G.add_edge_e graph new_edge
              ) incoming_to_merge;
            (*We remove the mixer *)
            G.iter_succ_e (fun e ->
                let (_,p) = G.E.label e in
                let dst = G.E.dst e in
                let new_edge = Flowgraph.G.E.create incoming_resampler (1,p) dst in
                G.add_edge_e graph new_edge) graph vertex;
            (*Remove the mixer node. We cannot do it directly due to limitationso fthe implentation and how it interacts with the traversal. So we remove it later. *)
            (*G.remove_vertex graph vertex;*)
            Enum.push vertices_to_remove vertex
          end;
      end
  in
  let merge_resamplers_after vertex =
    let hashtbl = Hashtbl.create (G.out_degree graph vertex) in
    let to_merge e  =
      let lblDst = G.V.label (G.E.dst e) in
      let input_port,_ = G.E.label e in
      if Flowgraph.(lblDst.className = "resampler") then Hashtbl.add hashtbl input_port (G.E.dst e)
    in
    G.iter_succ_e to_merge graph vertex;
    let port_cluster = Enum.map (fun key -> Hashtbl.find_all hashtbl key) (Enum.uniq (Hashtbl.keys hashtbl)) in
    (* Should aim at factorizing that *)
    let merge outcoming_to_merge =
      let outcoming_length = List.length outcoming_to_merge in
      if outcoming_length > 1 then
        begin
          let first_resampler = G.V.label (List.hd outcoming_to_merge) in
          let outcoming_resampler = G.V.create {id="res" ^(string_of_int (unique_id ()));
                                               nb_inlets=1; nb_outlets=outcoming_length; className="resampler";
                                               text=None ; wcet=Some 0. ; more=[("ratio", List.assoc "ratio" first_resampler.more)] } in
          G.add_vertex graph outcoming_resampler;
          List.iteri (fun i v ->
              let outcoming_edge = G.succ_e graph v in
              assert(1 = List.length outcoming_edge );
              let dst = G.E.dst (List.hd outcoming_edge) in
              G.remove_vertex graph v;
              let new_edge = G.E.create outcoming_resampler (i+1, 1) dst in
              G.add_edge_e graph new_edge
            ) outcoming_to_merge;
          let incoming_edge = Flowgraph.G.E.create vertex  (1,1) outcoming_resampler in
          G.add_edge_e graph incoming_edge
        end
    in
    Enum.iter merge port_cluster
  in
  let module Traversal = Traverse.Dfs(G) in
  Traversal.prefix merge_resamplers_before graph;
  Enum.iter (G.remove_vertex graph) vertices_to_remove;
  Traversal.prefix merge_resamplers_after graph (*Seems to remove some edges that should not be removed. TODO*)


let graph_to_ratio_graph graph = Mapper.map (fun edge -> let (pi, po) = Flowgraph.G.E.label edge in
                                              (Flowgraph.G.E.src edge, (pi, ref 1., po), Flowgraph.G.E.dst edge) ) graph

(* Pick a resampler among a list of vertices *)
let pick_resampler = List.find_opt (fun vertex -> Flowgraph.(vertex.className = "resampler"))

(* graph is the audio graph
   durations : node -> float is the WCET/AET of each computation node in s
   resamplerDuration : factor -> float is the time in s to resample by the factor
   budget is the available time budget for the whole graph
*)
let dowsample_components graph durations resamplerDuration budget =
  let ratio_graph = graph_to_ratio_graph graph in
  let schedule = get_schedule ratio_graph in
  let nb_tasks = Array.length schedule in
  let remaining_duration = Array.fold_left (fun sum node -> sum +. (durations node)) 0. schedule in
  (*let cumulative_durations = Array.create_float nb_tasks in
  cumulative_durations.(0) <- durations (fst schedule.(0));
  for i=1 to nb_tasks - 1 do
    cumulative_durations.(i) <- cumulative_durations.(i-1) +. durations (fst (schedule.(i)));
    done;*)
  if remaining_duration > budget then (* Problem, not enough time to execute everything*)
    (* Find where to degrade *)
    Printf.printf "We are going to degrade!\n";
    let rec find_where_to_degrade i durations_left durations_right =
      if i >= 0 then
        let current_node = schedule.(i) in
        let current_duration = durations current_node in
        let durations_left = durations_left -. current_duration in
        let durations_right = durations_right +. current_duration in
        (* Degrading right *)
        let total_duration = durations_left +. durations_right /. 2. in
        if total_duration <= budget then
          i - 1 (* because we insert the resampler before i, so after i - 1 *)
        else
          find_where_to_degrade (i - 1) durations_left durations_right
      else
        0
    in
    let node_to_degrade = find_where_to_degrade (nb_tasks - 1) remaining_duration 0. in
    ignore (exhaustive_heuristic ratio_graph schedule node_to_degrade);
    ratio_graph_to_graph ratio_graph graph
