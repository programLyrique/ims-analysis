(** To export a graph to the audiograph file format (.ag)
*)
open Batteries
open Flowgraph

let strize str = "\"" ^ str ^ "\""
(*Check if it does not start by a number (forbidden for indents in the def of the format). If it does, start by a letter*)
let identize id = if Char.is_digit id.[0] then "v" ^id else id

let out_attribute indent output (name, value) = Printf.fprintf output "%s%s : %s," indent name value
let out_vertex output vertex =
  let node = G.V.label vertex in
  let ident = identize node.id in
  let attributes = List.map (fun (name, value) -> (name, strize value)) node.more in
  (* nb_inlets : int ; nb_outlets : int; className : string ; text : string option ;
               wcet : float option;*)
  let attributes = Option.map_default (fun t ->  ("text", strize t)::attributes )  attributes node.text in
  let attributes = Option.map_default (fun t ->  ("wcet", string_of_float t)::attributes )  attributes node.wcet in
  let attributes = ("in", string_of_int node.nb_inlets)::("out", string_of_int node.nb_outlets)
                   ::("kind", strize node.className)::attributes in
  Printf.fprintf output "%s = " ident;
  List.print ~first:"{\n" ~last:"};\n" ~sep:"\n" (out_attribute "\t") output attributes

let out_edge output edge =
  let (src_port, dst_port) = G.E.label edge  in
  let src = G.E.src edge and dst = G.E.dst edge in
  let src_ident = identize (G.V.label src).id and dst_ident = identize (G.V.label dst).id in
  Printf.fprintf output "%s.%d -> %s.%d;\n" src_ident src_port dst_ident dst_port

let to_output flowgraph output =
  G.iter_vertex (out_vertex output) flowgraph;
  G.iter_edges_e (out_edge output) flowgraph

let export filename flowgraph =
  let filename = filename ^ ".ag" in
  File.with_file_out filename (to_output flowgraph)
