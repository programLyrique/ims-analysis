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
  | Any of string
[@@deriving show]

type pdwindow =
  | MainWindow of position * size * int
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
  | Any s -> (s, None)
  | _ -> failwith "Unrecognized object. Certainly a connection."

let build_node ident line =
  (* We only want to display the classname and the text. We can't really know how
     many inlets and outlets there are, only the number of used ones*)
  let className, text = extract_descriptions line in
  { id = string_of_int ident; nb_inlets = -1; nb_outlets = -1; className; text}

let build_graph patch =
  (* We first associate to every object an id and vice-versa. The id is their order
     of appearance as objects (excluding connections) in the file, as an integer *)
  let patch = Array.of_list patch in
  (* There are at most as many objects in the patch as there are elements...
     but more likely strictly less *)
  let id = ref (-1) in
  let build_node2 = function
    | Pdobject (Connect(_,_,_,_ )) -> None
    | Pdobject e -> incr id; Some (build_node !id e)
    | _ -> None
  in
  let nodes = Array.filter_map build_node2 patch  in
  let size = Array.length nodes in
  let graph = G.create ~size:size () in
  Array.iter (fun node -> G.add_vertex graph node) nodes;
  let edges = Array.filter_map (function Pdobject(Connect(s, i, d, j)) -> Some (Connect(s, i, d, j)) | _ -> None) patch in
  let add_edge e =
    let Connect(s,i,d, j) = e in
    let v1 =  nodes.(s) in
    let v2 = nodes.(d) in
    let edge = (v1, i * 10 + j, v2) in
    G.add_edge_e graph edge
  in
  Array.iter add_edge edges;
  graph
