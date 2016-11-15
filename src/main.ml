(** A maxpat file is a json file. *)

open Batteries
open Yojson
open Printf

let main() =
  let maxpat_file = Sys.argv.(1) in

  if not (String.ends_with maxpat_file ".maxpat") then
    begin
      print_endline "Wrong format";
      exit 1;
    end;

  let json = Safe.from_file maxpat_file in
  let open Yojson.Safe.Util in
  let fileversion = json |> member "patcher" |> member "fileversion" |> to_int in
  Printf.printf "File version is %d\n" fileversion

let () = main()
