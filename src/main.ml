(** A maxpat file is a json file. *)

open Batteries
open Yojson
open Printf
open Graph

type node = {id : string; nb_inlets : int ; nb_outlets : int; className : string ; text : string option }

type connection = {source_node : string ; source_port : int ;
                   destination_node : string ; destination_port : int}

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

(* Needed modules to build a graph representing a patch *)
module Node = struct
  type t = node
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module Edge = struct
  type t = int
  let compare = Pervasives.compare
  let equal = (=)
  let default = 0
end

module G = Imperative.Digraph.ConcreteLabeled(Node)(Edge)

let remove_quotes = String.replace_chars (fun c -> if c = '\"' then "" else Char.escaped c)

module Dot = Graphviz.Dot(struct
    include G
    let edge_attributes e = [] (*let open Graphviz.DotAttributes in  [`Label (string_of_int e)] *)
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [`Shape `Box ; `Label  (if v.className = "newobj" then remove_quotes (Option.default "newobj"  v.text) else v.className) ]
    let vertex_name v = "\"" ^ v.id ^ "\""
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end )


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


let main() =
  let maxpat_file = Sys.argv.(1) in

  if not (String.ends_with maxpat_file ".maxpat") then
    begin
      print_endline "Wrong format";
      exit 1;
    end;

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
  print_endline "Generating dot graph";
  let graph = build_graph boxes lines in
  let file = Pervasives.open_out_bin (maxpat_file ^".dot") in
  Dot.output_graph file graph

let () = main()
