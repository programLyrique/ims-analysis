(* Representation fo a dtaflow graph of Max or Puredata *)

open Batteries
open Graph



type node = {id : string; nb_inlets : int ; nb_outlets : int; className : string ; text : string option ; more : (string * string) list}
[@@deriving show]

type connection = {source_node : string ; source_port : int ;
                   destination_node : string ; destination_port : int}
[@@deriving show]




(* Needed modules to build a graph representing a patch *)
module Node = struct
  type t = node
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let empty = {id=""; nb_inlets=0; nb_outlets=0; className=""; text=None; more=[] }
  let is_valid n = not (n.id = "" || n.className = "")
end

module Edge = struct
  type t = int*int
  let compare = Pervasives.compare
  let equal = (=)
  let default = (0,0)
end

module G = Imperative.Digraph.ConcreteLabeled(Node)(Edge)


let build_graph nodes edges =
  let size = List.length nodes in
  let hashtbl = Hashtbl.create size in
  List.iter (fun node -> Hashtbl.add hashtbl node.id node) nodes;
  let graph = G.create ~size:size () in
  List.iter (fun node -> G.add_vertex graph node) nodes;
  let add_edge e =
    let v1 = Hashtbl.find hashtbl e.source_node in
    let v2 = Hashtbl.find hashtbl e.destination_node in
    let edge = G.E.create v1 (e.source_port, e.destination_port) v2 in
    G.add_edge_e graph edge
  in
  List.iter add_edge edges;
  graph

let remove_quotes = String.replace_chars (fun c -> if c = '\"' then "" else Char.escaped c)

module Dot = Graphviz.Dot(struct
    include G
    let edge_attributes e = [] (*let open Graphviz.DotAttributes in  [`Label (string_of_int e)] *)
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v =
      (`Shape `Box) ::
      match v.className with
      | "newobj" -> [`Label (remove_quotes (Option.default "newobj"  v.text))]
      | "comment" -> [`Label (remove_quotes (Option.get v.text))]
      | "message" -> [`Label (Option.get v.text)]
      | _ -> [`Label v.className]

    let vertex_name v = "\"" ^ v.id ^ "\""
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end )
