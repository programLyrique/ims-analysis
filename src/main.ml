(** A maxpat file is a json file. *)

open Batteries
open Yojson
open Printf

type node = {id : string; nb_inlets : int ; nb_outlets : int; className : string}

type connection = {source_node : string ; source_port : int ;
                   destination_node : string ; destination_port : int}

let to_node js =
  let open Yojson.Safe.Util in
   {
     id = js |> member "id" |> to_string;
     nb_inlets = js |> member "numinlets" |> to_int ;
     nb_outlets = js |> member "numoutlets" |> to_int;
     className = js |> member "maxclass" |> to_string;
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
  print_endline "Finished"

let () = main()
