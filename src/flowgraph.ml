(* Representation fo a audio graph *)

open Batteries
open Graph



type node = {id : string; nb_inlets : int ; nb_outlets : int; className : string ; text : string option ;
             wcet : float option;
              more : (string * string) list}
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
  let equal_content (n1 : t) (n2 : t) = {n1 with id=""} = {n2 with id=""}
  let empty = {id=""; nb_inlets=0; nb_outlets=0; className=""; text=None ; wcet=None; more=[] }
  let is_valid n = not (n.id = "" || n.className = "")
  let make id nb_inlets nb_outlets className =
    {id; nb_inlets; nb_outlets; className; text=None ; wcet=None; more=[] }
end

module Edge = struct
  type t = int*int
  let compare = Pervasives.compare
  let equal = (=)
  let default = (0,0)
end

(* We need abstract labeled here because two nodes could have the same label *)
module G = struct
  include Imperative.Digraph.AbstractLabeled(Node)(Edge)
  let format_edge  edge =
    let src = V.label (E.src edge) and dst = V.label (E.dst edge) in
    let (i, o) = E.label edge in
    Printf.sprintf "(%s, (%d, %d), %s)\n" (show_node src)  i o (show_node dst)
  let format_graph graph = fold_edges_e (fun edge s -> Printf.sprintf "%s%s" s (format_edge edge)) graph ""

  (** Replace destination in an edge*)
  let replace_dest graph v e =
    let new_edge = E.create (E.src e) (E.label e) v in
    add_edge_e graph new_edge;
    (*Remove old edge*)
    remove_edge_e graph e

  (** Replace src in an edge*)
  let replace_src graph v e =
    let new_edge = E.create v (E.label e) (E.dst e) in
    add_edge_e graph new_edge;
    (*Remove old edge*)
    remove_edge_e graph e

  (** The map_vertex of ocamlgraph is bugged... *)
  let map_vertex f graph =
    let n_graph = create ~size:(nb_vertex graph) () in
    let hashtbl = Hashtbl.create (nb_vertex graph) in

    iter_vertex (fun v ->
        let new_vertex = f v in
        Hashtbl.add hashtbl (V.label v).id new_vertex; add_vertex n_graph new_vertex)
      graph;
        let n = ref 0 in
        iter_edges_e (fun e ->
            incr n;
        let src = Hashtbl.find hashtbl (V.label (E.src e)).id in
        let dst = Hashtbl.find hashtbl (V.label (E.dst e)).id in
        let new_edge = E.create src (E.label e) dst in
        add_edge_e n_graph new_edge;
        assert(mem_edge_e n_graph new_edge)
      ) graph;
    (*assert (nb_vertex graph = nb_vertex n_graph);
    Printf.printf "%d %d %d" (nb_edges graph) (nb_edges n_graph) !n;
      assert (nb_edges graph = nb_edges n_graph);*)
    n_graph
end

module TopoStable =Topological.Make_stable(G)
let equal t1 t2 =
  let vertices1 = Array.of_list (List.rev (TopoStable.fold (fun node l -> node::l) t1 [])) in
  let vertices2 = Array.of_list (List.rev (TopoStable.fold (fun node l -> node::l) t2 [])) in
  try
    if Array.length vertices1 <> Array.length vertices2 then raise Exit;
    for i= 0 to Array.length vertices1 - 1 do
      if G.V.label vertices1.(i) <> G.V.label vertices2.(i) then raise Exit;
      let edges1 = List.map (fun edge -> (G.V.label (G.E.dst edge), G.E.label edge)) (G.succ_e t1 vertices1.(i)) in
      let edges2 = List.map (fun edge -> (G.V.label (G.E.dst edge), G.E.label edge)) (G.succ_e t2 vertices2.(i)) in
      if edges1 <> edges2 then raise Exit;
    done;
    true
  with Exit -> false

module Topo = Topological.Make(G)

module TraverseDfs = Traverse.Dfs(G)

let equal_content t1 t2 =
  let vertices1 = Array.of_list (List.rev (TopoStable.fold (fun node l -> node::l) t1 [])) in
  let vertices2 = Array.of_list (List.rev (TopoStable.fold (fun node l -> node::l) t2 [])) in
  try
    if Array.length vertices1 <> Array.length vertices2 then (print_endline "plop3";raise Exit);
    for i= 0 to Array.length vertices1 - 1 do
      if not (Node.equal_content (G.V.label vertices1.(i)) (G.V.label vertices2.(i))) then (print_endline "plop1";raise Exit);
      let edges1 = List.map (fun edge -> (G.V.label (G.E.dst edge), G.E.label edge)) (G.succ_e t1 vertices1.(i)) in
      let edges2 = List.map (fun edge -> (G.V.label (G.E.dst edge), G.E.label edge)) (G.succ_e t2 vertices2.(i)) in
      if not (List.fold_left2 (fun b (l1, e1) (l2, e2) -> b && (Node.equal_content l1 l2) &&  e1 = e2) true edges1  edges2 ) then (print_endline"plop2";raise Exit);
    done;
    true
  with Exit -> false

(* Check if inlets and oulets are coherent *)
let coherent_iolets graph =
  G.iter_vertex (fun v ->
      assert( G.in_degree graph v = (G.V.label v).nb_inlets);
      assert( G.out_degree graph v = (G.V.label v).nb_outlets)
    ) graph

(* Nodes which are not on edges are not added to the graph *)
let build_graph nodes edges =
  let size = List.length nodes in
  let hashtbl = Hashtbl.create size in
  List.iter (fun node -> Hashtbl.add hashtbl node.id (G.V.create node)) nodes;
  let graph = G.create ~size:size () in
  let add_edge e =
    let v1 = Hashtbl.find hashtbl e.source_node in
    let v2 = Hashtbl.find hashtbl e.destination_node in
    let edge = G.E.create v1 (e.source_port, e.destination_port) v2 in
    G.add_edge_e graph edge
  in
  List.iter add_edge edges;
  graph

module ExtG = struct
  include G
  let empty () = create ()
  let add_edge_e t edge = add_edge_e t edge ; t
  let add_vertex t vertex = add_vertex t vertex ; t
end

module EdgeMapper = Gmap.Edge(G)(ExtG)
module NodeMapper = Gmap.Vertex(G)(ExtG)

let remove_quotes = String.replace_chars (fun c -> if c = '\"' then "" else Char.escaped c)

module Dot = Graphviz.Dot(struct
    include G
    let edge_attributes e =
      let (pi,po) = G.E.label e in
      [`Headlabel ("i"^string_of_int po); `Taillabel ("o"^string_of_int pi); `Arrowsize 0.5]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v =
      let v = G.V.label v in
      (`Shape `Box) ::
      match v.className with
      | "newobj" -> [`Label (v.id ^ remove_quotes (Option.default "newobj"  v.text))]
      | "comment" -> [`Label (v.id ^remove_quotes (Option.get v.text))]
      | "message" -> [`Label (v.id ^Option.get v.text)]
      | "resampler" -> [`Label (v.id^"resampler " ^ (List.assoc "ratio" v.more))]
      | _ -> [`Label (v.id ^v.className)]

    let vertex_name v = let v = G.V.label v in "\"" ^ v.id ^ "\""
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end )
