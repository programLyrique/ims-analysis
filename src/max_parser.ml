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
    wcet = Some 0.;
    more = []
  }

let to_connection js =
  let open Yojson.Safe.Util in
  let source = js |> member "source"
  and destination = js |> member "destination" in
  {
    source_node = source |> index 0 |> to_string;
    source_port = 1 + (source |> index 1 |> to_int);
    destination_node = destination |> index 0 |> to_string;
    destination_port = 1 + (destination |> index 1 |> to_int);
  }



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
