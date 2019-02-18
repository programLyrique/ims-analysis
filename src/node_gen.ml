(**From an audiograph description file, extract nodes and gen possible nodes using a type system.
   A type indication starts with @.
   @[attr]set

   attr: pick | all
   set: [start,end] | {a1,a2,...}

   pick means that we only choose one possibility (randomly?) among what is available
   all means that we should chose everything (in case of an interval, we will choose an unspecified sampling interval)
*)

open Batteries

type enumerative = Pick | All [@@deriving show]
type possibilities = Interval of float * float | Set of string list [@@deriving show]

type attr_type = {enum: enumerative; possibilities: possibilities} [@@deriving show]


let parse_interval chars =
  assert ((Enum.get_exn chars) = '[');
  let start = float_of_string (String.of_enum (Enum.take_while (fun v -> v <> ',') chars)) in
  Enum.junk chars; (*Remove the ","*)
  let ending = float_of_string (String.of_enum ( Enum.take_while (fun v -> v <> ']') chars)) in
  Enum.junk chars;
  Interval (start, ending)

let parse_set chars =
  assert ((Enum.get_exn chars) = '{');
  let set = Enum.take_while (fun v -> v <> '}') chars in
  let rec parse_elems ch =
    if Enum.is_empty ch then []
    else
      begin
        let (elem, tail) = Enum.span (fun v -> v <> ',') ch in
        Enum.junk tail; (*get rid of that , if there is one *)
        (String.of_enum elem)::(parse_elems tail)
      end
  in
  Set (parse_elems set)


let parse_attr attr =
  (*Let's parse by hand! *)
  let chars = String.enum attr in
  let default_attr = {enum=All;possibilities=Set [attr]} in
  Option.map_default (fun v -> match v with
      | '@' ->
        begin
          Enum.junk chars;
          let enumer,values= Enum.span (fun c -> c <> '[' && c <> '{') chars in
          let enumer = match String.of_enum enumer with
            | "pick" -> Pick
            | "all" -> All
            | _ -> failwith ("Not a valid type: " ^ attr)
          in
          let values = match Option.get (Enum.peek chars ) with
            | '[' -> parse_interval chars
            | '{' -> parse_set chars
            | _ -> failwith ("Not a valid type: " ^ attr)
          in
          {enum=enumer;possibilities=values}
        end
      | _ -> default_attr
      )
      default_attr (Enum.peek chars)

let parse_node_attrs node =
  let open Flowgraph in
  let suppl_attrs = node.more in
  List.map (fun (k, v) -> (k, parse_attr v)) suppl_attrs

let gen_nodes_from_attr node parsed_attrs =
  ()


(** Load an audiograph file which we use as a dictionnary of possible audio effects. We get a hashtbl (nb_in, nb_out) : node *)
let load_possible_nodes filename =
  let f = File.open_in filename in
  let lexbuf = Lexing.from_channel f  in
  Lexing.(lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename });
  let nodes,_,_ = Audiograph_lexer.parse_with_error_ag lexbuf in
  let hashtbl = Hashtbl.create (List.length nodes) in
  let open Flowgraph in
  List.iter (fun node -> Hashtbl.add hashtbl (node.nb_inlets, node.nb_outlets) node) nodes;
  hashtbl
