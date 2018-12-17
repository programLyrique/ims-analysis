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
  let vertices = G.fold_vertex (fun v l -> v::l ) graph [] in
  let all_subsets = superset vertices in
  let rec enumerate prev_graph =
    List.iter (fun subset ->
        List.iter (fun v ->
        let graph = G.copy prev_graph in
        (*Insert downsampler on all outputs *)
        let successors = insert_resamplers graph v 0.5 in
        (*update_markings graph successors 0.5;*)
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
      subset)
  all_subsets
  in
  enumerate graph;
  !res
