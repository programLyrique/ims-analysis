open Flowgraph
open Batteries

type position = {x : int [@default 0]; y : int [@default 0]}
[@@deriving show, make]

type size = {w : int [@default 0]; h : int [@default 0]}
[@@deriving show, make]

type kind = Graph | Pd
[@@deriving show]

type pdobject =
  | Connect of int * int * int * int
  | Obj of position * string * string
  | Msg of position * string
  | Text of position * string
  | Floatatom of position * int * int * int
  | Restore of position * kind * string option
  | Canvas of string
  | Any of string
[@@deriving show]

type pdwindow =
  | MainWindow of position * size * int
  | Subpatch of position * size * string
[@@deriving show]

type patch_statement = Pdarray of float array | Pdwindow of pdwindow | Pdobject of pdobject
[@@deriving show]


type patch = patch_statement list
[@@deriving show]

let extract_descriptions  = function
  | Obj(_, classname, args) -> ("newobj", Some (classname ^" " ^args))
  | Msg(_, s) -> ("message", Some s)
  | Text(_, s) -> ("comment", Some s)
  | Floatatom(_,i,_,_) -> ("floatatom", None)
  | Canvas s -> ("pd", Some s)
  | Any s -> (s, None)
  | _ -> failwith "Unrecognized object. Certainly a connection."

let build_node scope_name ident line =
  (* We only want to display the classname and the text. We can't really know how
     many inlets and outlets there are, only the number of used ones*)
  let className, text = extract_descriptions line in
  G.V.create { id = scope_name ^ string_of_int ident; nb_inlets = -1; nb_outlets = -1; className; text ; wcet=None; more = []}


type scope = { name: string;
               scope_node: G.V.t [@printer fun fmt n -> fprintf fmt "%s" (show_node (G.V.label n))];
               scope_inlets: G.V.t list [@printer fun fmt l -> fprintf fmt "%d" (List.length l)];
               scope_outlets: G.V.t list [@printer fun fmt l -> fprintf fmt "%d" (List.length l)];
             }
[@@deriving show]
let add_inlet_scope scope inlet = {scope with scope_inlets = inlet::scope.scope_inlets}
let add_outlet_scope scope outlet = {scope with scope_outlets = outlet::scope.scope_outlets}
let build_new_scope name scope_node = {name; scope_node; scope_inlets=[]; scope_outlets=[] }



(**Merge an incoming edge with outcoming edges from one inlet node*)
let merge_e_dest graph v edge =
  (*Printf.printf "Inlet: %s\n" (show_node (G.V.label v));*)
  let src = G.E.src edge in
  let (i,_) = G.E.label edge in
  assert (G.in_degree graph v = 0);
  G.iter_succ_e (fun e ->
      let (_,o) = G.E.label e in
      let new_edge = G.E.create src (i,o) (G.E.dst e) in
      G.add_edge_e graph new_edge;
      (*G.remove_edge_e graph e (*No need for it as removing v removes all connected edges to it*) *)
    ) graph v;
  (*We will remove the node later*)
  G.remove_edge_e graph edge

(**Merge an incoming edge with outcoming edges from one inlet node*)
let merge_e_src graph v edge =
  (*Printf.printf "Outlet: %s\n" (show_node (G.V.label v));*)
  let dst = G.E.dst edge in
  let (_,o) = G.E.label edge in
  assert (G.out_degree graph v = 0);
  G.iter_pred_e (fun e ->
      let (i,_) = G.E.label e in
      let new_edge = G.E.create (G.E.src e) (i,o) dst in
      G.add_edge_e graph new_edge;
      (*G.remove_edge_e graph e (*No need for it as removing v removes all connected edges to it*) *)
    ) graph v;
  (*We will remove the node later*)
  G.remove_edge_e graph edge

(** To connect the inlets and outlets of a subpatch to the parents connections. Will remove the inlet and outlet boxes. *)
let merge_subpatch graph scope =
  (*Printf.printf "Scope: %s\n" (show_scope scope);*)

  (*If the node in the parent was an orphan, and we don't have chosen to preserve orphans, the node does not exist in the graph *)
  if G.mem_vertex graph scope.scope_node then
    begin
      let nb_in_parent = G.in_degree graph scope.scope_node in
      let nb_out_parent = G.out_degree graph scope.scope_node in
      let nb_inlets = List.length scope.scope_inlets in
      let nb_outlets = List.length scope.scope_outlets in

      (*We consider that the left to right order of inlets/outlets is a top to bottom order in the file.
        To check... and we actually have the position of the nodes on the patch *)

      (*Dealing with inlets*)
      if nb_in_parent > 0 && nb_inlets > 0 then
        begin
          let parent_inlets = G.pred_e graph scope.scope_node in
          let scope_inlets = List.rev scope.scope_inlets in (* Actually, here they are nodes and not vertices *)
          let parent_inlets = if nb_in_parent > nb_inlets then
              begin
                (*We connect the supplementary ones on the first inlet. *)
                let prefix, parent_inlets = List.split_at (nb_in_parent - nb_inlets) parent_inlets in
                let first_inlet = List.hd scope_inlets in
                List.iter (merge_e_dest graph first_inlet) prefix;
                parent_inlets
              end
            else (*Less parents in the parent patch than there are inlets in the subpatch *)
              begin
                (*In that case we will connect the last parent inlet several times, to the remaining inlets*)
                parent_inlets @ (List.make (nb_inlets - nb_in_parent) (List.last parent_inlets))
              end in
          assert ((List.length scope_inlets) = (List.length parent_inlets));
          List.iter2 (merge_e_dest graph) scope_inlets parent_inlets;
          (*Remove the inlet nodes *)
          List.iter (G.remove_vertex graph) scope_inlets;
        end;

      (*Outlets *)
      if nb_out_parent > 0 && nb_outlets > 0 then
        begin
          let parent_outlets = G.succ_e graph scope.scope_node in
          let scope_outlets = List.rev scope.scope_outlets in
          let parent_outlets = if nb_out_parent > nb_outlets then
              begin
                (*We connect the supplementary ones onto the first outlet. *)
                let prefix, parent_outlets = List.split_at (nb_out_parent - nb_outlets) parent_outlets in
                let first_outlet = List.hd scope_outlets in
                List.iter (merge_e_src graph first_outlet) prefix;
                parent_outlets
              end
            else
              begin
                parent_outlets @ (List.make (nb_outlets - nb_out_parent) (List.last parent_outlets))
              end in
          assert ((List.length scope_outlets) = (List.length parent_outlets));
          List.iter2 (merge_e_src graph) scope_outlets parent_outlets;
          (*Remove the outlet nodes *)
          List.iter (G.remove_vertex graph) scope_outlets;
        end
    end

(** Deal with listing objects and connecting them taking into account subpatches.
    keep_orphans keeps orphan nodes in the graph.
    connect_subpatches unfolds subpatches, so that the subpatch is replaced by its content.
    Inlets and outlets boxes are removed and replaced by proper connections. *)
let build_graph ?(keep_orphans=false) ?(connect_subpatches=false) patch_decl =
  let graph = G.create  () in
  let add_edge nodes e =
    let Connect(s,i,d, j) = e in
    let v1 =  DynArray.get nodes s in
    let v2 = DynArray.get nodes d in
    let edge = G.E.create v1 (i+1,j+1) v2 in
    G.add_edge_e graph edge
  in
  let add_to_graph nodes connections =
    (* We don't add the nodes, but only the edges, which will remove any orphan nodes, except if keep_orphans is true *)
    if keep_orphans then
      DynArray.iter (G.add_vertex graph) nodes;
    List.iter (add_edge nodes) connections in
  (*i is the line number inside a subpatch, scope_name is the name of the scope (canvas)
    objects is a dynarray gathering the nodes in order of apparition, connections is a list of connections.

    Returns a tuple with 1st element a list of the scopes already seen (and alredy processed) and 2nd, a list of remaining lines to process
  *)
  let rec process_scope i scope objs connections = function
    | (Pdwindow Subpatch (_,_, name)::l) as canvas ->
      let subpatch_node = build_node scope.name i (Canvas name) in
      DynArray.add objs subpatch_node;
      new_scope i scope objs connections canvas
    | (Pdobject (Restore(_, Graph, _)))::l -> (Vect.empty, l) (*The graph is not a scope *)
    | (Pdobject (Connect(s, i, d, j) as c))::l  -> process_scope i  scope objs (c::connections) l
    | (Pdarray _ )::l -> (*We ignore it*) process_scope i scope objs connections l
    | (Pdobject (Restore(_, Pd, name)))::l ->
      add_to_graph objs connections;
      (Vect.singleton scope, l)
    | (Pdobject obj)::l -> let node = build_node scope.name i obj in
      DynArray.add objs node;
      let scope = match obj with
        | Obj(_,"inlet", _) | Obj(_,"inlet~", _) -> add_inlet_scope scope node
        | Obj(_, "outlet", _) | Obj(_, "outlet~", _) -> add_outlet_scope scope node
        | _ -> scope in
      process_scope (i+1)  scope objs connections l
    | [] -> (*No more lines! Time to add the nodes in the main patch!! *)
      assert (scope.name = "main");
      (*We don't add the main scope to the list of scopes *)
      add_to_graph objs connections;(Vect.empty,[])
    | _ -> failwith (Printf.sprintf "Unexpected declaration in scope %s\n" scope.name)
  and new_scope i current_scope objs connections  lines =
    let name  = match List.hd lines with
      | Pdwindow Subpatch (_,_, name)   -> name
      | _ -> failwith "MainWindow should not be here.\n" in
    (* Create and process new scope *)
    let scope = build_new_scope name (DynArray.last objs) in
    let scopes, remaining_lines = process_scope 0  scope (DynArray.create ()) [] (List.tl lines) in
    (*Restore previous scope *)
    let next_scopes, lines = process_scope (i+1) current_scope objs connections remaining_lines in
    (*Adding the new scopes to the previous ones.  *)
    (Vect.concat scopes next_scopes, lines)
  in
  let main_scope = build_new_scope "main" (build_node "" 0 (Any "")) in
  let scopes,_ = match (List.hd patch_decl) with
    | Pdwindow (MainWindow _ ) -> process_scope 0  main_scope (DynArray.create ()) [] (List.tl patch_decl)
    | _ -> failwith "Expected main window declaration at the beginning of the file" in

  if connect_subpatches then
    begin
      Vect.iter (merge_subpatch graph) scopes
    end;

  (*Correct inlet and outlet numbers*)
  let n_graph = G.map_vertex (fun node ->
      (*let max_input_port = G.fold_pred_e (fun e m -> let (_,p) = G.E.label e in max m p ) graph node 0 in
        let max_output_port = G.fold_succ_e (fun e m -> let (p,_) = G.E.label e in max m p ) graph node 0 in*)
      let label = G.V.label node in
      G.V.create {label with nb_inlets = G.in_degree graph node; nb_outlets = G.out_degree graph node}
    )  graph in
  assert (G.nb_edges graph = G.nb_edges n_graph);
  Flowgraph.coherent_iolets n_graph;
  n_graph


(** Only works for patch without subpatches *)
let build_graph2 patch =
  (* We first associate to every object an id and vice-versa. The id is their order
     of appearance as objects (excluding connections) in the file, as an integer *)
  let patch = Array.of_list patch in
  (* There are at most as many objects in the patch as there are elements...
     but more likely strictly less *)
  let id = ref (-1) in
  let build_node2 = function
    | Pdobject (Connect(_,_,_,_ )) -> None
    | Pdobject e -> incr id; Some (build_node "" !id e)
    | _ -> None
  in
  let nodes = Array.filter_map build_node2  patch  in
  let size = Array.length nodes in
  let graph = G.create ~size:size () in
  (*Ports start at 1 in our model *)
  let edges = Array.filter_map (function Pdobject(Connect(s, i, d, j)) -> Some (Connect(s, i + 1, d, j + 1)) | _ -> None) patch in
  let add_edge e =
    let Connect(s,i,d, j) = e in
    assert (i > 0 && j > 0 );
    let v1 =  nodes.(s) in
    let v2 = nodes.(d) in
    let edge = G.E.create v1 (i,j) v2 in
    G.add_edge_e graph edge
  in
  Array.iter add_edge edges;
  (*Correct inlet and outlet numbers*)
  let n_graph = G.map_vertex (fun node ->
      (*let max_input_port = G.fold_pred_e (fun e m -> let (_,p) = G.E.label e in max m p ) graph node 0 in
        let max_output_port = G.fold_succ_e (fun e m -> let (p,_) = G.E.label e in max m p ) graph node 0 in*)
      let label = G.V.label node in
      G.V.create {label with nb_inlets = G.in_degree graph node; nb_outlets = G.out_degree graph node}
      )  graph in
    assert (G.nb_edges graph = G.nb_edges n_graph);
    n_graph
