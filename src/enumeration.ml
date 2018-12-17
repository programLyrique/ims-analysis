(** Enumerating all possible degraded versions of an audio graph *)
open Graph
open Batteries
open Flowgraph

(* We need abstract labeled here because two nodes could have the same label *)
module G = struct
  include Imperative.Digraph.ConcreteLabeled(Node)(Edge)
  let format_edge  edge =
    let src = V.label (E.src edge) and dst = V.label (E.dst edge) in
    let (i, o) = E.label edge in
    Printf.sprintf "(%s, (%d, %d), %s)\n" (show_node src)  i o (show_node dst)
  let format_graph graph = fold_edges_e (fun edge s -> Printf.sprintf "%s%s" s (format_edge edge)) graph ""
end

(* To sample DAGs: Uniform random generation of large acyclic digraphs; Jack Kuipers, Giusi Moffa *)

(*Enumerate ordered pairs among an array A^n_2, from index k, apply a function f on the pairs and collect the results *)
let enumerate_pairs tab k f =
  for i = k to Array.length tab - 1 do
    for j = i +1  to Array.length tab - 1 do
      f tab.(i) tab.(j);
      f tab.(j) tab.(i);
    done
  done

(**List all directed unlabelled acyclic graphs with n nodes/vertices and all vertices are connected *)
(*TODO: use matrix representation of ocamlgraph? *)
(*let dags n =
  (* Create n vertices *)
  let vertices = Array.init n (fun i -> G.V.create (Node.make (string_of_int i)  (-1) (-1) "test")) in

  (*Enumerate all possible edges. *)
  let rec enumerate_edges k =
    if k < n then
      for i=k to n - 1 do
        enumerate_pairs vertices i (enumerate_edges )

      done;
*)

(*Apply a function f on all the subsets of size of tab *)
(*let iter_subsets f tab =
  let n = Array.length tab in
  let pow_set_size  = int_of_float (2. ** (float_of_int n)) in

  for i = 0 to pow_set_size - 1 do
    let res = ref [] in
    for j= 0 to n - 1 do
      if (i land (1 lsl j) = 1) then
        begin
          Printf.printf "%d" tab.(j);
          res := tab.(j) :: !res
        end
    done;
    Printf.printf "\n";
    f !res
  done
*)

(*Compute the superset and apply a function on each element *)
let rec superset = function
  | [] ->  [[]]
  | x :: xs ->
    let ps = superset xs  in
    ps @ List.map (fun ss -> x :: ss) ps




(*Insert resamplers after node in graph for all its outputs *)
let insert_resamplers graph node ratio =
  let node_lbl = G.V.label node in
  let node_id = node_lbl.id in
  (*Get outcoming edges and nodes. *)
  assert (G.mem_vertex graph node);
  let outcoming_edges = G.succ_e graph node in
  let nb_outcomings = List.length outcoming_edges in
  (*Remove edges*)
  List.iter (fun e -> G.remove_edge_e graph e) outcoming_edges;

  (* Create resamplers *)
  let resampler_nodes = List.init nb_outcomings (fun i ->
      G.V.create {id="res" ^node_id^ "-" ^(string_of_int i); nb_inlets=1; nb_outlets=1; className="resampler"; text=None ; wcet=Some 0.; more=[("ratio", string_of_float ratio)] }
    )
  in
  (*Insert resamplers *)
  List.iter2 (fun edge resampler_node ->
    let (pi, po) = G.E.label edge in
    (* Here, i and o really store original vertices from the original graph so we are not creating fresh vertices*)
    let e1 = G.E.create (G.E.src edge) (pi, 1) resampler_node in
    let e2 = G.E.create resampler_node (1, po) (G.E.dst edge) in
    G.add_edge_e graph e1;
    G.add_edge_e graph e2
    ) outcoming_edges resampler_nodes;
  (*We return the list of successors of the resamplers *)
  List.map G.E.dst outcoming_edges

(* Updates list of marks of successors of the nodes. Stop on a branch if we reach another resampler. Would require the AbstractLabeled version of the graph *)
(*let update_markings graph nodes ratio =
(* Update markings of successors : if 0<= ratio < 1 then -1/ratio, else ratio *)
  let marking = int_of_float (if ratio < 1.0 then -1. /. ratio else ratio) in
  let rec mark_ratio vertex =
    if (G.V.label vertex).className <> "resampler" then
      G.iter_succ (fun v -> G.Mark.set v marking; mark_ratio v) graph vertex
  in
  List.iter mark_ratio nodes
*)

let is_next_upsampler v = (G.V.label v).className = "resampler"



(*Enumerate all possible degraded versions.
  For now, with only one downsamplers.
  TODO: with several downsamplers, up to the number of nodes
*)
let enumerate_degraded_versions graph =
  let res = ref [] in
  let add_graph g = res := g::!res in
  let rec enumerate prev_graph =
  G.iter_vertex (fun v ->
        let graph = G.copy prev_graph in
        (*Insert downsampler on all outputs *)
        let successors = insert_resamplers graph v 0.5 in
        let rec insert_upsamplers vertex =
          let graph = G.copy graph in
          let succs = insert_resamplers graph vertex 2. in
          List.iter (fun v ->
              add_graph graph;
              if not (is_next_upsampler v) then
                insert_upsamplers v
            )
            succs
        in
        List.iter insert_upsamplers successors
      )
      prev_graph
  in
  enumerate graph;
  !res


(*Simpler way of enumerating degraded versions:
  we compute the powerset of the set of edges and each degraded version
  corresponds to a subsets of the power set where all the edges of the subset are degraded.
  We ust need to exclude the edges which go to an output because we cannot insert
  an upsampler after an output *)

module Edge = struct
  type t = int*float*int (*input port, flow rate compared to reference rate, output port *)
  let compare = Pervasives.compare
  let equal = (=)
  let default = (0,1.,0)
end

module Gf = struct
  include Imperative.Digraph.ConcreteLabeled(Node)(Edge)
  let format_edge  edge =
    let src = V.label (E.src edge) and dst = V.label (E.dst edge) in
    let (i, flow, o) = E.label edge in
    Printf.sprintf "(%s, (%d, %f, %d), %s)\n" (show_node src)  i flow o (show_node dst)
  let format_graph graph = fold_edges_e (fun edge s -> Printf.sprintf "%s%s" s (format_edge edge)) graph ""
end

let complement_set set subset = List.filter (fun e -> not (List.mem e subset )) set

let complement_vertices set subset = List.filter (fun v1 ->
    let lbl1 = Gf.V.label v1 in
      List.exists (fun v2 -> let lbl2 = Gf.V.label v2 in lbl2.id = lbl1.id ) subset ) set

let is_edge_degraded e = let (_,flow,_) = Gf.E.label e in flow = 0.5

(*Insert a resampler on an edge *)
let insert_resampler_e graph e ratio =
  let src = Gf.E.src e and dst = Gf.E.dst e in
  let src_id = (Gf.V.label src).id in
  let dst_id = (Gf.V.label dst).id in
  let (pi, _, po) = Gf.E.label e in

  let resampler = G.V.create {id="res-" ^src_id^ "-" ^dst_id; nb_inlets=1; nb_outlets=1; className="resampler"; text=None ; wcet=Some 0.; more=[("ratio", string_of_float ratio)] } in
  let e1 = G.E.create src (pi, 1) resampler in
  let e2 = G.E.create resampler (1, po) dst in
  G.add_edge_e graph e1;
  G.add_edge_e graph e2


let graph_to_graphflow graph =
  let graph_c = G.create ~size:(Gf.nb_vertex graph) () in
  Gf.iter_edges_e (fun e ->
      (* We want all incoming edges to be isochronous *)
      let pred_edges = Gf.pred_e graph (Gf.E.src e) in
      let pred_degraded = List.for_all is_edge_degraded pred_edges in
      if (List.is_empty pred_edges || not pred_degraded) && is_edge_degraded e then
        begin
          insert_resampler_e graph_c e 0.5
        end
      else if not (List.is_empty pred_edges) && pred_degraded && not (is_edge_degraded e) then
        begin
          insert_resampler_e graph_c e 2.
        end
      else
        begin
          let (pi, _ , po) = Gf.E.label e in
          G.add_edge_e graph_c (G.E.create (Gf.E.src e ) (pi,po) (Gf.E.dst e))
        end
    )
    graph;
  graph_c


(*Does not work because all output edges must be degraded because of isochrony *)
let enumerate_degraded_versions_edges graph =
  let edges = Gf.fold_edges_e (fun e l -> e::l) graph [] in
  let edges_powerset = superset edges in
  let set_to_graph subset =
    let graph = Gf.copy graph in
    let complement = complement_set edges subset in
    List.iter (fun e -> Gf.add_edge_e graph e) complement;
    List.iter (fun e ->
        let (i,_ ,o) = Gf.E.label e in
        let edge = Gf.E.create (Gf.E.src e) (i, 0.5, o) (Gf.E.dst e) in
        Gf.add_edge_e graph edge)
      subset;
    graph
  in
  List.map set_to_graph edges_powerset

(*If it's an edge going to an output*)
let is_final_edge graph e = let dst = Gf.E.dst e in (Gf.out_degree graph dst) = 0

let show_vertices = List.iter (fun v -> Printf.printf "%s\n" (show_node v))

let enumerate_degraded_versions_vertex graph =
  let vertices = Gf.fold_vertex (fun v l -> v::l) graph [] in
  let vertices_not_final = List.filter (fun v -> (Gf.out_degree graph v) > 0) vertices in
  let vertex_powerset = superset vertices_not_final in
  let set_to_graph subset =
    let graph_c = Gf.create ~size:(Gf.nb_vertex graph) () in
    let complement = complement_set vertices subset in
    (*Printf.printf "Complement: ";
    show_vertices complement; Printf.printf "\nSubset: ";
      show_vertices subset; Printf.printf "\n";*)
    List.iter (fun v -> Gf.iter_succ_e (fun e -> Gf.add_edge_e graph_c e) graph v) complement;
    List.iter (fun v ->
        (*If there is a final edge, we cannot insert downsamplers on that edge *)
      let f =  if List.exists (is_final_edge graph) (Gf.succ_e graph v) then
           (fun e -> Gf.add_edge_e graph_c e)
      else
        begin (fun e ->
              let (i,_ ,o) = Gf.E.label e in
              let edge = Gf.E.create (Gf.E.src e) (i, 0.5, o) (Gf.E.dst e) in
              Gf.add_edge_e graph_c edge)
        end in
      Gf.iter_succ_e f graph v) subset;
    assert ((Gf.nb_vertex graph_c ) >= (Gf.nb_vertex graph));
    graph_c
  in
  let degraded_versions = List.map set_to_graph vertex_powerset in
  (*Remove all graphs where isochronous conditions on inputs is not respected *)
  let is_isochronous graph =
    Gf.fold_vertex (fun v b -> let preds = Gf.pred_e graph v in
                     b && (if not (List.is_empty preds) then let (_, flow1, _) = Gf.E.label (List.hd preds) in
                             List.for_all (fun e -> let (_,flow2,_) = Gf.E.label e in flow1 = flow2) preds else true) )
      graph true
  in
  let degraded_versions = Set.of_list degraded_versions in
  let degraded_versions = Set.filter is_isochronous degraded_versions in
  let degraded_versions = Set.map graph_to_graphflow degraded_versions in
  Set.to_list degraded_versions

module TempGf = struct
  include Gf
  let empty () = Gf.create ~size:0 ()
  let add_edge_e t edge = Gf.add_edge_e t edge; t
end

module GToGf = Gmap.Edge(G)(TempGf)

let graphflow_to_graph = GToGf.map (fun e -> let (pi, po) = G.E.label e in Gf.E.create (G.E.src e) (pi, 1.0, po) (G.E.dst e))

module FlowgraphToGf = Gmap.Edge(Flowgraph.G)(TempGf)

let flowgraph_to_graphflow = FlowgraphToGf.map (fun e ->
    let open Flowgraph in
    let (pi, po) = G.E.label e in Gf.E.create (G.V.label (G.E.src e)) (pi, 1.0, po) (G.V.label (G.E.dst e))
  )

module TempFlowgraph = struct
  include Flowgraph.G
  let empty () = Flowgraph.G.create ~size:0 ()
  let add_edge_e t edge = Flowgraph.G.add_edge_e t edge; t
end

module GToFlowgraph = Gmap.Edge(G)(TempFlowgraph)

let graph_to_flowgraph graph =
  let hashtbl = Hashtbl.create (G.nb_vertex graph ) in
  G.iter_vertex (fun v -> Hashtbl.add hashtbl  v.id (Flowgraph.G.V.create  v)) graph;
  let graph_c = Flowgraph.G.create ~size:(G.nb_vertex graph) () in
  G.iter_edges_e (fun e -> let (pi, po) = G.E.label e in
                             let src = Hashtbl.find hashtbl (G.E.src e).id in
                             let dst = Hashtbl.find hashtbl (G.E.dst e).id in
                 Flowgraph.G.add_edge_e graph_c (Flowgraph.G.E.create src (pi, po) dst))
      graph;
  assert ((G.nb_vertex graph ) = (Flowgraph.G.nb_vertex graph_c));
  graph_c
