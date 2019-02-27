(**From an audiograph description file, extract nodes and gen possible nodes using a type system.
   A type indication starts with @.
   @[attr]set

   attr: pick | all
   set: [start,end] | {a1,a2,...}

   pick means that we only choose one possibility (randomly?) among what is available
   all means that we should chose everything (in case of an interval, we will choose an unspecified sampling interval)
*)

open Batteries
open Graph

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

let pick = function
  | Interval(start, ending) -> string_of_float (start +. (Random.float (ending -. start)))
  | Set l -> Random.choice (List.enum l)

let all = function
  | Interval(start, ending) ->
    let nb_elems = 50 in
    Enum.init nb_elems (fun i -> string_of_float (start +. (ending -. start) /. (float_of_int nb_elems) *. (float_of_int i)))
  | Set l -> List.enum l

let gen_nodes_from_attr node parsed_attrs =
  let open Flowgraph in
  let update_node node name attr = Enum.map (fun node -> {node with more=(name, attr)::node.more}) in
  let rec next_nodes cur_nodes = function
    | (name, {enum=Pick;possibilities})::l ->  update_node node name (pick possibilities) cur_nodes
    | (name, {enum=All;possibilities})::l -> Enum.flatten (Enum.map (fun attr -> update_node node name attr cur_nodes) (all possibilities))
    | _ -> cur_nodes
  in
  next_nodes (Enum.singleton {node with more=[]}) parsed_attrs

let gMixers = Global.empty "mixers"
let gResamplers = Global.empty "resamplers"

let get_wcet_resampler () =
  let resampler = Option.bind (Global.get gResamplers) (fun l -> List.at_opt l 0)  in
  Option.bind resampler (fun n -> Flowgraph.(n.wcet))

let get_wcet_mixer nb_inlets nb_outlets =
  let mixers = Global.get gMixers in
  let mixer = Option.bind mixers (fun hashtbl -> Hashtbl.find_option hashtbl (nb_inlets, nb_outlets)) in
  let open Flowgraph in
  if Option.is_none mixer && Option.is_some mixers then
      let hashtbl = Option.get mixers in
      let mix_1_1 = Option.bind (Hashtbl.find_option hashtbl (1,1)) (fun n -> n.wcet) in
      let mix_2_1 = Option.bind (Hashtbl.find_option hashtbl (2,1)) (fun n -> n.wcet) in
      let mix_1_2 = Option.bind (Hashtbl.find_option hashtbl (1,2)) (fun n -> n.wcet) in
      if Option.is_some mix_1_1 && Option.is_some mix_1_2 && Option.is_some mix_2_1 then
        let mix_1_1 = Option.get mix_1_1 in
        let mix_2_1 = Option.get mix_2_1 in
        let mix_1_2 = Option.get mix_1_2 in
        let add_wcet = mix_2_1 -. mix_1_1 in
        let copy_wcet = mix_1_2 -. mix_1_1 in
        Some (float_of_int nb_inlets *. add_wcet +. float_of_int nb_outlets *. copy_wcet)
      else None
    else None

(** Load an audiograph file which we use as a dictionnary of possible audio effects. We get a hashtbl (nb_in, nb_out) : node *)
let load_possible_nodes filename =
  let f = File.open_in filename in
  let lexbuf = Lexing.from_channel f  in
  Lexing.(lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename });
  let open Flowgraph in
  let nodes,_,_ = Audiograph_lexer.parse_with_error_ag lexbuf in
  (*assert (List.for_all (fun node -> 0. < Option.default 0. node.wcet) nodes);*)
  let nodes,special_nodes=List.partition (fun n -> Flowgraph.(n.className <> "mix" && n.className <> "resampler")) nodes in
  let mixers, resamplers=List.partition (fun n -> Flowgraph.(n.className = "mix")) special_nodes in
  let mixers_hash = Hashtbl.create (List.length mixers) in
  List.iter (fun node -> Hashtbl.add mixers_hash (node.nb_inlets, node.nb_outlets) node) mixers ;
  Global.set gMixers mixers_hash;
  Global.set gResamplers resamplers;
  let parsed_attrs = List.map (fun n -> (n, parse_node_attrs n)) nodes in
  let gen_nodes = List.map (fun (n,attrs) -> gen_nodes_from_attr n attrs) parsed_attrs in
  let hashtbl = Hashtbl.create (List.length gen_nodes) in
  let gen_nodes = Enum.flatten (List.enum gen_nodes) in
  Enum.iter (fun node -> Hashtbl.add hashtbl (node.nb_inlets, node.nb_outlets) node) gen_nodes;
  hashtbl

let mixer nb_inlets nb_outlets = Flowgraph.({className="mix"; nb_inlets; nb_outlets; id="";wcet=get_wcet_mixer nb_inlets nb_outlets ;text=None;more=[] })

(** Randomly pick a node among the possible ones*)
let pick_node id nb_in nb_out node_table =
  let nodes = Hashtbl.find_all node_table (nb_in, nb_out) in
  let node = if List.is_empty nodes then mixer nb_in nb_out
    else   Random.choice (List.enum nodes) in
  Flowgraph.({node with id=id})

(**Modifies a fake node by one picked in the node table *)
let real_node node_table graph n =
  let open Flowgraph in
  let nb_inlets = G.in_degree graph n in
  let nb_outlets = G.out_degree graph n in
  (*Here, we can pick any node  which as maximum nb_outlets. If it has less, some ports can go to several nodes. *)
  let nb_outlets = if nb_outlets = 0 then 0 else 1 + Random.int nb_outlets in
  let id = (G.V.label n).id in
  G.V.create (pick_node id nb_inlets nb_outlets node_table)

(** Generate one possible graph given a node table*)
let gen_possible_graph node_table graph =
  let open Flowgraph in
  let g = G.map_vertex (fun v -> real_node node_table graph v) graph in
  (*Correct output ports *)
  (*TODO: Instead of creating a new graph, remvoe an edge and replace it by the right one?*)
  let hashtbl = Hashtbl.create (G.nb_vertex g ) in
  let graph_c = Flowgraph.G.create ~size:(G.nb_vertex g) () in
  G.iter_vertex (fun v ->
      let new_v = (G.V.create  (G.V.label v)) in
      G.add_vertex graph_c new_v;
      Hashtbl.add hashtbl  (G.V.label v).id new_v) g;
  G.iter_vertex (fun v ->
      let nb_outlets = (G.V.label v).nb_outlets in
      if nb_outlets > 0 then (*If there are no outlets, there are no successors as well so anyway we would not iter *)
        let i = ref 1 in
        G.iter_succ_e (fun e ->
            let src = Hashtbl.find hashtbl (G.V.label (G.E.src e)).id in
            let dst = Hashtbl.find hashtbl (G.V.label (G.E.dst e)).id in
            let (pi,po) = G.E.label e in
            G.add_edge_e graph_c (G.E.create src (!i, po) dst);
            i := 1 + (!i  mod nb_outlets)
          )
        g v
    ) g;
  assert ((G.nb_vertex g ) = (Flowgraph.G.nb_vertex graph_c));
  (*Correct output ports*)
  G.iter_vertex (fun v ->
      let i = ref 1 in
      G.iter_pred_e (fun e ->
          let src = G.E.src e in
          let dst = G.E.dst e in
          let (pi,po) = G.E.label e in
          let new_edge = G.E.create src (pi, !i) dst in
          G.remove_edge_e graph_c e;
          G.add_edge_e graph_c new_edge;
          incr i
        ) graph_c v
    ) graph_c;
  graph_c



(** From one graph, generate all possible versions with the given node table*)
let gen_possible_graphs node_table graph =
  ()
