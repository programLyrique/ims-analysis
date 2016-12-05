(* To parse a maxpat file *)

open Batteries
open Yojson
open Flowgraph

let to_node js =
  let open Yojson.Safe.Util in
  {
    id = js |> member "id" |> to_string;
    nb_inlets = js |> member "numinlets" |> to_int ;
    nb_outlets = js |> member "numoutlets" |> to_int;
    className = js |> member "maxclass" |> to_string;
    text = js |> member "text" |> to_string_option;
  }

let to_connection js =
  let open Yojson.Safe.Util in
  let source = js |> member "source"
  and destination = js |> member "destination" in
  {
    source_node = source |> index 0 |> to_string;
    source_port = source |> index 1 |> to_int;
    destination_node = destination |> index 0 |> to_string;
    destination_port = destination |> index 1 |> to_int;
  }


  let build_graph nodes edges =
    let size = List.length nodes in
    let hashtbl = Hashtbl.create size in
    List.iter (fun node -> Hashtbl.add hashtbl node.id node) nodes;
    let graph = G.create ~size:size () in
    List.iter (fun node -> G.add_vertex graph node) nodes;
    let add_edge e =
      let v1 = Hashtbl.find hashtbl e.source_node in
      let v2 = Hashtbl.find hashtbl e.destination_node in
      let edge = (v1, e.source_port * 10 + e.destination_port, v2) in
      G.add_edge_e graph edge
    in
    List.iter add_edge edges;
    graph

let parse_maxpat maxpat_file =
  let json = Safe.from_file maxpat_file in
  print_endline "Processing";
  let open Yojson.Safe.Util in
  let boxes = json |> member "patcher"  |> member "boxes"   |> map (member "box")
              |> to_list |> List.map to_node in
              (* |> map (member "text") |> to_list |> filter_string *)
  let lines = json |>  member "patcher" |> member "lines" |> map (member "patchline")
              |> to_list |> List.map to_connection in
  Printf.printf "Boxes are:\n %s\n" (dump boxes);
  Printf.printf "Lines are:\n %s\n" (dump lines);
  build_graph boxes lines
