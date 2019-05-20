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
  type t = Flowgraph.G.V.t
  let compare = Flowgraph.Node.compare
  let hash n = Hashtbl.hash Flowgraph.((G.V.label n).id) (*We actually don't care about the bool that was previously second member of this type as a tuple. And the ref would create problems with the hash function anyway *)
  let equal n1 n2 = Flowgraph.Node.equal n1 n2
  let empty : t = Flowgraph.G.V.create Flowgraph.Node.empty
  let is_valid n = Flowgraph.(not (n.id = "" || n.className = ""))
  let is_on_resampled_path tbl node = Hashtbl.find_default tbl node false
  let on_resampled_path tbl node = Hashtbl.replace tbl node true
  let to_flowgraph_node node =  node
end

let nb_resamplers graph =
  let open Flowgraph in
  G.fold_vertex (fun v nb -> if (G.V.label v).className = "resampler" then nb + 1 else nb) graph 0


let make_resampler_node id  ratio =
  let open Flowgraph in
   {id; nb_inlets=1; nb_outlets=1; className="resampler"; text=None ; wcet=None; more=[("ratio", string_of_float ratio)] }

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
  Printf.printf "fntd = %d ; nb_tasks = %d\n" first_node_to_degrade nb_tasks;
  assert (0 <= first_node_to_degrade && first_node_to_degrade <= nb_tasks - 1 );

  (*Inserting downsamplers here*)

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

    (*Is it a sink node? *)
    if Flowgraph.((G.V.label current_node).nb_outlets = 0) then
      begin
        if Node.is_on_resampled_path hashtbl current_node then
          begin
            G.iter_pred_e (fun edge -> if Node.is_on_resampled_path hashtbl (G.E.src edge) then Edge.resample (G.E.label edge) 2.) graph current_node
          end
      end
    else
      begin
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
        end
      end
    (*Printf.printf "$$ Modified graph is currently: %s \n" (format_graph graph);*)
  done


(*One step degradation. Nb nodes steps is 0-quality, 0 steps is 1-quality. nb_predecessors gives the number of predecessor nodes of a given node *)
let step_degrade graph nb_predecessors active_nodes =
  (* Pick node we are going to degrade before *)
  if not (List.is_empty active_nodes) then
    begin
    let node = List.fold_right (fun node min_node ->
        let nb_pred_min = Hashtbl.find nb_predecessors min_node in
        let nb_pred = Hashtbl.find nb_predecessors node in if nb_pred < nb_pred_min then node else min_node)
        active_nodes
        (List.hd active_nodes)
    in
    (* all other outputs must be undegraded if they previously were. *)
    let active_nodes = List.remove active_nodes node in
    List.iter (fun n -> G.iter_succ_e (fun (_, (_, r, _), _) -> r := 1.) graph n) active_nodes;
    (*Degrade  *)
    G.iter_pred_e (fun (_, (_, r, _), _) -> r := 0.5) graph node;
    (*Add inputs of chosen node to active nodes *)
    (G.pred graph node) @ active_nodes
    end
  else []


(* Degrade by trying to minimize the number of degraded nodes in the whole graph, and traversing the graph from outputs. It does not actually find the minimum node path... *)
let minimizing_strategy graph outputs nb_to_degrade =
  (*Need to do a first computation of the number of predecessors of any nodes and store that in each node. *)
  let nb_predecessors = Hashtbl.create (G.nb_vertex graph)  in
  (*We could compute it forward also... *)
  let rec traverse_backwards vertex =
    let nb_pred = G.fold_pred (fun v nb -> nb + traverse_backwards v) graph vertex 0 in
    Hashtbl.add nb_predecessors vertex nb_pred; nb_pred in
  ignore (List.map traverse_backwards outputs);
  let active_nodes = ref outputs in
  for i=1 to nb_to_degrade do
    active_nodes := step_degrade graph nb_predecessors !active_nodes
  done



let get_ratio more =
  float_of_string (List.assoc "ratio" more)



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
        let resampler_node = Flowgraph.(G.V.create {id="res" ^(string_of_int (unique_id ())); nb_inlets=1; nb_outlets=1; className="resampler"; text=None ; wcet=None; more=[("ratio", string_of_float ratio)] }) in
        let (pi, _, po) = label in
        (* Here, i and o really store original vertices from the original graph so we are not creating fresh vertices*)
        let e1 = Flowgraph.G.E.create (Node.to_flowgraph_node i) (pi, 1) resampler_node in
        let e2 = Flowgraph.G.E.create resampler_node (1, po) (Node.to_flowgraph_node o) in
        Flowgraph.G.add_edge_e graph e1;
        Flowgraph.G.add_edge_e graph e2
      end
  in
  Traversal.prefix (G.iter_succ_e insert_resampler ratio_graph) ratio_graph

(** Check if resamplers are well inserted and defined *)
let check_resamplers graph =
  let open Flowgraph in
  G.iter_vertex (fun vertex ->
      let label = G.V.label vertex in
      if label.className = "resampler" then
        begin
          assert(G.in_degree graph vertex = 1)
        end
    ) graph



(* Can merge common channels with sample resampling ratio but inserting new channels and only one resampler.
       If merging incoming:
        - R --\                         --\
        - R -- mixer -- e1    becomes   -- mixer -- R -- e1
        - R --/                         --/
   Or:
          /-- R - e1                   /-- e1
       e.p -- R - e2         e.p -- R - -- e3
          \-- R - e3                   \-- e3

   This pass should be done one the ratio graph or on the flowgraph?...
   Will be more efficient on ratio graph when merging incoming, because access to predecessors is O(1).
*)
let merge_resamplers graph =
  let open Flowgraph in
  (*let graph_c = G.copy graph in*)
  let vertices_to_remove = Enum.empty () in
  let edges_to_add = Enum.empty () in
  let merge_resamplers_before vertex =
    (* Merge incoming. Here we suppose that we use only two resampling ratio.
       TODO: copy the graph, because the insertion of nodes here interferes with the traversal.
    *)
    (*We remove the mixer *)
    (* If the current node is a mixer, we can merge incoming nodes *)
    let to_merge v l = let lbl = G.V.label v in if Flowgraph.(lbl.className = "resampler") then v::l else l in

    if Flowgraph.((G.V.label vertex).className = "mix") then
      begin
        let incoming_to_merge  = G.fold_pred to_merge graph vertex [] in
        let incoming_length = List.length incoming_to_merge in
        if incoming_length > 1 then
          begin
            (*If there are only resamplers after, don't do the transformation *)
            let all_resampler_after = G.fold_succ (fun v b -> b && (G.V.label v).className = "resampler") graph vertex true in
            if not all_resampler_after then
              begin
                let first_resampler = G.V.label (List.hd incoming_to_merge) in
                assert(List.for_all (fun res -> get_ratio first_resampler.more = get_ratio (G.V.label res).more ) incoming_to_merge);
                let incoming_resampler = G.V.create {id="res_in" ^(string_of_int (unique_id ()));
                                                 nb_inlets=1; nb_outlets=1; className="resampler";
                                                     wcet=Node_gen.get_wcet_resampler ();
                                                 text=None ; more=[("ratio", List.assoc "ratio" first_resampler.more)] } in
                let new_mixer = G.V.create (G.V.label vertex) in
                (*G.add_vertex graph_c new_mixer;*)
                (*check_resamplers graph_c;
                  G.add_vertex graph incoming_resampler;*)
                List.iteri (fun i v ->
                    let incoming_edge = G.pred_e graph v in
                    assert(1 = List.length incoming_edge );
                    let src = G.E.src (List.hd incoming_edge) in
                    (*G.remove_vertex graph v;*)
                    Enum.push vertices_to_remove v;
                    let new_edge = G.E.create src (1, i+1) new_mixer in
                    (*G.add_edge_e graph_c new_edge*)
                    Enum.push edges_to_add new_edge
                  ) incoming_to_merge;
                (*List.iter (G.remove_vertex graph_c) incoming_to_merge;*)
                (*Edge between new mixer and new resampler*)
                let edge_mix_res = Flowgraph.G.E.create new_mixer (1,1) incoming_resampler in
                (*G.add_edge_e graph_c edge_mix_res;*)
                Enum.push edges_to_add edge_mix_res;
                G.iter_succ_e (fun e ->
                let (_,p) = G.E.label e in
                let dst = G.E.dst e in
                let new_edge = Flowgraph.G.E.create incoming_resampler (1,p) dst in
                (*G.add_edge_e graph_c new_edge*)
                Enum.push edges_to_add new_edge
                  ) graph vertex;

                (*Remove the old mixer node. We cannot do it directly due to limitations of the implentation and how it interacts with the traversal. So we remove it later. *)
                (*G.remove_vertex graph vertex*)
                Enum.push vertices_to_remove vertex
              end;
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
    let port_cluster = Enum.map (fun key -> (key, Hashtbl.find_all hashtbl key)) (Enum.uniq (Hashtbl.keys hashtbl)) in
    (* Should aim at factorizing that *)
    let merge (port, outcoming_to_merge) =
      let outcoming_length = List.length outcoming_to_merge in
      if outcoming_length > 1 then
        begin
          let first_resampler = G.V.label (List.hd outcoming_to_merge) in
          let outcoming_resampler = G.V.create {id="res_out" ^(string_of_int (unique_id ()));
                                               nb_inlets=1; nb_outlets=1; className="resampler";
                                                text=None ; wcet=Node_gen.get_wcet_resampler () ; more=[("ratio", List.assoc "ratio" first_resampler.more)] } in
          (*G.add_vertex graph_c outcoming_resampler;*)
          let incoming_edge = Flowgraph.G.E.create vertex  (port,1) outcoming_resampler in
          (*G.add_edge_e graph_c incoming_edge;*)
          Enum.push edges_to_add incoming_edge;
          List.iter (fun  v ->
              let outcoming_edge = G.succ_e graph v in
              assert(1 = List.length outcoming_edge );
              let dst = G.E.dst (List.hd outcoming_edge) in
              let _,dst_port = G.E.label (List.hd outcoming_edge) in
              (*G.remove_vertex graph_c v;*)
              Enum.push vertices_to_remove v;
              let new_edge = G.E.create outcoming_resampler (1, dst_port) dst in
              (*G.add_edge_e graph_c new_edge*)
              Enum.push edges_to_add new_edge
            ) outcoming_to_merge;
        end
    in
    Enum.iter merge port_cluster
  in
  let module Traversal = Traverse.Dfs(G) in
  Traversal.prefix merge_resamplers_before graph;
  (*Enum.iter (G.remove_vertex graph) vertices_to_remove;
  Enum.iter (G.add_edge_e graph) edges_to_add;
    check_resamplers graph;*)
  Traversal.prefix merge_resamplers_after graph;
  Enum.iter (G.remove_vertex graph) vertices_to_remove;
  Enum.iter (G.add_edge_e graph) edges_to_add;
  check_resamplers graph;
  graph


let graph_to_ratio_graph graph = Mapper.map (fun edge -> let (pi, po) = Flowgraph.G.E.label edge in
                                              (Flowgraph.G.E.src edge, (pi, ref 1., po), Flowgraph.G.E.dst edge) ) graph

(* Pick a resampler among a list of vertices *)
let pick_resampler = List.find_opt (fun vertex -> Flowgraph.(vertex.className = "resampler"))

(* graph is the audio graph
   durations : node -> float is the WCET/AET of each computation node in s
   resamplerDuration : factor -> float is the time in s to resample by the factor
   budget is the available time budget for the whole graph
*)
let downsample_components graph durations resamplerDuration budget =
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
    let rec find_where_to_degrade i durations_left durations_right =
      if i > 0 then
        let current_node = schedule.(i) in
        let current_duration = durations current_node in
        let durations_left = durations_left -. current_duration in
        let durations_right = durations_right +. current_duration in
        (* Degrading right *)
        (*We just estimate here the overhead due to resamplers *)
        let total_duration = durations_left +. durations_right /. 2. +. 2. *. resamplerDuration in
        if total_duration <= budget then
          i - 1 (* because we insert the resampler before i, so after i - 1 *)
        else
          find_where_to_degrade (i - 1) durations_left durations_right
      else
        0
    in
    let node_to_degrade = find_where_to_degrade (nb_tasks - 1) remaining_duration 0. in
    ignore (exhaustive_heuristic ratio_graph schedule node_to_degrade);
    (*Todo: add upsampler node also in the exhaustive strategy *)
    ratio_graph_to_graph ratio_graph graph
