(** Testing node_gen *)
open OUnit2
open Batteries
open Node_gen

let parse_interval_t test_ctxt =
  let interval = String.enum "[23,45]" in
  assert_equal  (Interval (23., 45.)) (parse_interval interval)

let parse_set_t test_ctxt =
  let set = String.enum "{plop,bla,34}" in
  assert_equal ~printer:show_possibilities  (Set ["plop"; "bla"; "34"]) (parse_set set)

let parse_singleton_t test_ctxt =
  let set = String.enum "{trr}" in
  assert_equal ~printer:show_possibilities  (Set ["trr"]) (parse_set set)

let parse_pick test_ctxt =
  let attr =  "@pick[2,45.8]" in
  assert_equal ~printer:show_attr_type {enum=Pick;possibilities=Interval (2.,45.8)} (parse_attr attr)

let parse_all test_ctxt =
  let attr = "@all{plop}" in
  assert_equal ~printer:show_attr_type {enum=All;possibilities=Set ["plop"]} (parse_attr attr)

let parse_default test_ctxt =
  let attr ="salut" in
  assert_equal ~printer:show_attr_type {enum=All;possibilities=Set ["salut"]} (parse_attr attr)

let load_nodes test_ctxt =
  let nodes = load_possible_nodes "nodes.ag" in
  let filtered = Hashtbl.find_all nodes (1,1) in
  assert_equal ~printer:string_of_int 2 (List.length filtered)

let enumerate_connected_graphs_nodes test_tcx =
  let open Enumeration in
  let nodes = load_possible_nodes "nodes.ag" in
  let graphs = List.map (fun g -> gen_possible_graph nodes (graph_to_flowgraph_vertex g)) (gen_connected_directed_graphs 5) in

  (*List.iter (fun g -> Printf.printf "%s\n" (Enumeration.G.format_graph g)) graphs;*)
  assert_equal 848 ~printer:string_of_int (List.length graphs)

let suite = "node_gen" >::: ["parse_interval" >:: parse_interval_t;
                             "parse_set" >:: parse_set_t;
                             "parse_singleton" >:: parse_singleton_t;
                             "parse_pick" >:: parse_pick;
                             "parse_all" >:: parse_all;
                             "parse_default" >:: parse_default;
                             "load_nodes" >:: load_nodes
                            ];
