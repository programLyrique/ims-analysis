(** To export a graph to the audiograph file format (.ag)
*)
open Batteries
open Flowgraph

let out_attribute output (name, value) = Printf.fprintf output "%s : %s," name value
let out_vertex output vertex =
  let node = G.V.label vertex in
  let ident = node.id in
  let attributes = List.map (fun (name, value) -> (name, "\\\"" ^ value ^ "\\\"")) node.more in
  (* nb_inlets : int ; nb_outlets : int; className : string ; text : string option ;
               wcet : float option;*)
  let attributes = Option.map_default (fun t ->  ("text",t)::attributes )  attributes node.text in
  let attributes = Option.map_default (fun t ->  ("wcet", string_of_float t)::attributes )  attributes node.wcet in
  let attributes = ("in", string_of_int node.nb_inlets)::("out", string_of_int node.nb_outlets)
                   ::("kind", node.className)::attributes in
  Printf.fprintf output "%s = {\n" ident;
  List.print ~last:"};\n" ~sep:"\n" out_attribute output attributes

let out_edge output edge =
  let (src_port, dst_port) = G.E.label edge  in
  let src = G.E.src edge and dst = G.E.dst edge in
  let src_ident = (G.V.label src).id and dst_ident = (G.V.label dst).id in
  Printf.fprintf output "%s.%d -> %s.%d;" src_ident src_port dst_ident dst_port

let to_output output flowgraph =
  G.iter_vertex (out_vertex output) flowgraph;
  G.iter_edges_e (out_edge output) flowgraph

let export filename flowgraph =
  File.with_file_out filename to_output
