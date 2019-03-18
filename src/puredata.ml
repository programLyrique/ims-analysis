open Flowgraph
open Batteries

type position = {x : int ; y : int}
[@@deriving show]

type size = {w : int ; h : int}
[@@deriving show]

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
  { id = scope_name ^ string_of_int ident; nb_inlets = -1; nb_outlets = -1; className; text ; wcet=None; more = []}


(** Deal with listing objects and connecting them taking into account subpatches *)
let build_graph ?(keep_orphans=false) patch_decl =
  let graph = G.create  () in
  let add_edge nodes e =
    let Connect(s,i,d, j) = e in
    let v1 =  DynArray.get nodes s in
    let v2 = DynArray.get nodes d in
    let edge = G.E.create v1 (i+1,j+1) v2 in
    G.add_edge_e graph edge
  in
  let add_to_graph objs connections =
  (* Add the connections to the graph *)
  let nodes = DynArray.map G.V.create objs in
  (* We don't add the nodes, but only the edges, which will remove any orphan nodes, except if keep_orphans is true *)
  if keep_orphans then
    DynArray.iter (G.add_vertex graph) nodes;
  List.iter (add_edge nodes) connections in
  (*i is the line number inside a subpatch, scope_name is the name of the scope (canvas)
    objects is a dynarray gathering the nodes in order of apparition, connections is a list of connections.

    Returns a tuple with 1st element line of subpatch and then pair of classname and optiional text
  *)
  let rec process_scope i scope_name objs connections = function
    | (Pdwindow Subpatch (_,_, name)::l) as canvas ->
      DynArray.add objs (build_node scope_name i (Canvas name));
      new_scope i  scope_name objs connections canvas
    | (Pdobject (Restore(_, Graph, _)))::l -> l
    | (Pdobject (Connect(s, i, d, j) as c))::l  -> process_scope i  scope_name objs (c::connections) l
    | (Pdarray _ )::l -> (*We ignore it*) process_scope i scope_name objs connections l
    | (Pdobject (Restore(_, Pd, name)))::l ->
      add_to_graph objs connections;
      l
    | (Pdobject obj)::l -> let node = build_node scope_name i obj in
        DynArray.add objs node;
        process_scope (i+1)  scope_name objs connections l
    | [] -> (*No more lines! Time to add the nodes in the main patch!! *)
      assert (scope_name = "main");
      add_to_graph objs connections;[]
  and new_scope i scope_name objs connections  lines =
    let name  = match List.hd lines with
      | Pdwindow Subpatch (_,_, name)   -> name
      | _ -> failwith "MainWindow should not be here.\n" in
    (* Create new scope *)
    let remaining_lines = process_scope 0  name (DynArray.create ()) [] (List.tl lines) in
    (*Restore previous scope *)
    process_scope (i+1) scope_name objs connections remaining_lines
  in
  let () = match (List.hd patch_decl) with
    | Pdwindow (MainWindow _ ) -> ignore (process_scope 0  "main" (DynArray.create ()) [] (List.tl patch_decl))
    | _ -> failwith "Expected main window declaration at the beginning pf the file" in
  (*Correct inlet and outlet numbers*)
  (*NodeMapper.map (fun node ->
      let max_input_port = G.fold_pred_e (fun e m -> let (_,p) = G.E.label e in max m p ) graph node 0 in
      let max_output_port = G.fold_succ_e (fun e m -> let (p,_) = G.E.label e in max m p ) graph node 0 in
      let label = G.V.label node in
      G.V.create {label with nb_inlets = max_input_port; nb_outlets = max_output_port}
    )  graph*)
  graph


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
  let nodes = Array.map G.V.create nodes in
  let size = Array.length nodes in
  let graph = G.create ~size:size () in
  (*Ports start at 1 in our model *)
  let edges = Array.filter_map (function Pdobject(Connect(s, i, d, j)) -> Some (Connect(s, i + 1, d, j + 1)) | _ -> None) patch in
  let add_edge e =
    let Connect(s,i,d, j) = e in
    let v1 =  nodes.(s) in
    let v2 = nodes.(d) in
    let edge = G.E.create v1 (i,j) v2 in
    G.add_edge_e graph edge
  in
  Array.iter add_edge edges;
  (*Correct inlet and outlet numbers*)
  (*TODO: does not preserve edges. Fix!! *)
  NodeMapper.map (fun node ->
      let max_input_port = G.fold_pred_e (fun e m -> let (_,p) = G.E.label e in max m p ) graph node 0 in
      let max_output_port = G.fold_succ_e (fun e m -> let (p,_) = G.E.label e in max m p ) graph node 0 in
      let label = G.V.label node in
      G.V.create {label with nb_inlets = max_input_port; nb_outlets = max_output_port}
    )  graph
