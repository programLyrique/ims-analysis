(* testing functionalities of downsampling.ml *)
open OUnit2
open Batteries


let graph_to_ratio_graph test_ctxt =
  let open Flowgraph in
  let node1 = Flowgraph.G.V.create (Node.make "id-1" 1 1 "plop")  in
  let node2 = Flowgraph.G.V.create (Node.make "id-2" 1 1 "plop") in
  let node3 = Flowgraph.G.V.create (Node.make "id-3" 2 1 "mix") in
  let edge1 = Flowgraph.G.E.create node1 (1,1) node3 in
  let edge2 = Flowgraph.G.E.create node2 (1,2) node3 in
  let graph = Flowgraph.G.create ~size:3 () in
  Flowgraph.G.add_edge_e graph edge1;
  Flowgraph.G.add_edge_e graph edge2;
  assert_equal (G.nb_vertex graph) 3;
  assert_equal (G.nb_edges graph) 2;
  let ratio_graph = Downsampling.graph_to_ratio_graph graph in
  let ratio_graph_c = Downsampling.G.create ~size:3 () in
  let edge1_r = Downsampling.G.E.create (Downsampling.G.V.create node1) (1, ref 1., 1) (Downsampling.G.V.create node3) in
  let edge2_r = Downsampling.G.E.create (Downsampling.G.V.create node2) (1, ref 1., 2) (Downsampling.G.V.create node3) in
  ignore (Downsampling.G.add_edge_e ratio_graph_c edge1_r);
  ignore (Downsampling.G.add_edge_e ratio_graph_c edge2_r);
  assert_equal (Downsampling.G.nb_vertex ratio_graph) 3;
  assert_equal (Downsampling.G.nb_edges ratio_graph) 2;
  assert_equal  ~printer:Downsampling.format_graph ~cmp:Downsampling.G.equal ratio_graph ratio_graph_c


let ratio_graph_to_graph test_ctxt =
  let open Flowgraph in
  let node1 = Flowgraph.G.V.create (Node.make "id-1" 1 1 "plop")  in
  let node2 = Flowgraph.G.V.create (Node.make "id-2" 1 1 "plop") in
  let node3 = Flowgraph.G.V.create (Node.make "id-3" 2 1 "mix") in
  let edge1 = Flowgraph.G.E.create node1 (1,1) node3 in
  let edge2 = Flowgraph.G.E.create node2 (1,2) node3 in
  let graph = Flowgraph.G.create ~size:3 () in
  Flowgraph.G.add_edge_e graph edge1;
  Flowgraph.G.add_edge_e graph edge2;
  assert_equal (G.nb_vertex graph) 3;
  assert_equal (G.nb_edges graph) 2;
  let ratio_graph = Downsampling.G.create ~size:3 () in
  let node3_r = Downsampling.G.V.create node3 in
  let edge1_r = Downsampling.G.E.create (Downsampling.G.V.create node1) (1, ref 0.5, 1) node3_r in
  let edge2_r = Downsampling.G.E.create (Downsampling.G.V.create node2) (1, ref 0.5, 2) node3_r in
  ignore (Downsampling.G.add_edge_e ratio_graph edge1_r);
  ignore (Downsampling.G.add_edge_e ratio_graph edge2_r);
  let graph_c = G.copy graph in (* Will give new tags for vertices. So we need to use the original one or hack a bit on ctn_vertex. *)
  Downsampling.ratio_graph_to_graph ratio_graph graph;
  G.clear graph_c;
  let unique_id =
    let id  = ref 0 in
      function  () -> incr id; !id in
  (* Adding manually resampler to graph to compare with graph_c *)
  let resampler2 = G.V.create (Downsampling.make_resampler_node ("res" ^(string_of_int (unique_id ()))) 1 0.5)  in
  let resampler1 = G.V.create (Downsampling.make_resampler_node ("res" ^(string_of_int (unique_id ()))) 1 0.5)  in
  let e1 = G.E.create node1 (1,1) resampler1 and e2 = G.E.create node2 (1,1) resampler2 in
  let e1' = G.E.create resampler1 (1,1) node3 and e2' = G.E.create resampler2 (1,2) node3 in
  G.add_edge_e graph_c e1 ; G.add_edge_e graph_c e2;G.add_edge_e graph_c e1'; G.add_edge_e graph_c e2';

  assert_equal  ~printer:G.format_graph ~cmp:equal graph graph_c


let merge_resamplers_before test_ctxt =
  let open Flowgraph in
  (* Graph for which we are going to merge the resamplers *)
  let node1 = G.V.create (Node.make "id-1" 1 1 "plop")  in
  let node2 = G.V.create (Node.make "id-2" 1 1 "plop") in
  let node3 = G.V.create (Node.make "id-3" 2 1 "mixer") in
  let node4 = G.V.create (Node.make "id-4" 1 1 "effect") in
  let unique_id =
    let id  = ref 0 in
      function  () -> incr id; !id in
  let resampler2 = G.V.create (Downsampling.make_resampler_node ("res" ^(string_of_int (unique_id ()))) 1 0.5) in
  let resampler1 = G.V.create (Downsampling.make_resampler_node ("res" ^(string_of_int (unique_id ()))) 1 0.5) in
  let e1 = G.E.create node1 (1,1) resampler1 and e2 = G.E.create node2 (1,1) resampler2 in
  let e1' = G.E.create resampler1 (1,1) node3 and e2' = G.E.create resampler2 (1,2) node3 in
  let e = G.E.create node3 (1,1) node4 in
  let graph = G.create ~size:6 () in
  G.add_edge_e graph e1 ; G.add_edge_e graph e2;G.add_edge_e graph e1'; G.add_edge_e graph e2';
  G.add_edge_e graph e;
  (*Merging the edges *)
  Downsampling.merge_resamplers graph;
  (*Another graph where the resamplers are manually merged*)
  let resampler = G.V.create (Downsampling.make_resampler_node ("res" ^(string_of_int (unique_id ()))) 2 0.5) in
  let e1 = G.E.create node1 (1,1) resampler and e2 = G.E.create node2 (1,2) resampler in
  let e = G.E.create resampler (1,1) node4 in
  let manual_graph = G.create ~size:4 () in
  G.add_edge_e manual_graph e1;G.add_edge_e manual_graph e2;G.add_edge_e manual_graph e;

  assert_equal ~printer:G.format_graph ~cmp:equal_content manual_graph graph

  let merge_resamplers_after test_ctxt =
    let open Flowgraph in
    (* Graph for which we are going to merge the resamplers *)
    let node1 = G.V.create (Node.make "id-1" 1 1 "plop") in
    let node2 = G.V.create (Node.make "id-3" 1 1 "effect") in
    let node3 = G.V.create (Node.make "id-4" 1 1 "effect") in
    let unique_id =
      let id  = ref 0 in
        function  () -> incr id; !id in
    let resampler2 = G.V.create (Downsampling.make_resampler_node ("res" ^(string_of_int (unique_id ()))) 1 0.5) in
    let resampler1 = G.V.create (Downsampling.make_resampler_node ("res" ^(string_of_int (unique_id ()))) 1 0.5) in
    let e1 = G.E.create node1 (1,1) resampler1 and e2 = G.E.create node1 (1,1) resampler2 in
    let e1' = G.E.create resampler1 (1,1) node2 and e2' = G.E.create resampler2 (1,1) node3 in
    let graph = G.create ~size:5 () in
    G.add_edge_e graph e1 ; G.add_edge_e graph e2;G.add_edge_e graph e1'; G.add_edge_e graph e2';
    (*Merging the edges *)
    Downsampling.merge_resamplers graph;
    (*Another graph where the resamplers are manually merged*)
    let resampler = G.V.create {id="res" ^(string_of_int (unique_id ())); nb_inlets=1; nb_outlets=2; className="resampler"; text=None ; wcet=Some 0.; more=[("ratio", "0.5")] } in
    let e = G.E.create node1 (1,1) resampler in
    let e1 = G.E.create resampler (1,1) node2 and  e2 = G.E.create resampler (2,1) node3 in
    let manual_graph = G.create ~size:4 () in
    G.add_edge_e manual_graph e;G.add_edge_e manual_graph e1;G.add_edge_e manual_graph e2;

    assert_equal ~printer:G.format_graph ~cmp:equal_content manual_graph graph

let exhaustive_heuristic test_ctx =
  let open Downsampling in
  let graph, _,_ = Audiograph_parser_tests.parse_file "tests/downsampling_test.ag" in
  let ratio_graph = graph_to_ratio_graph graph in
  let target_ratio_graph = DeepCopy.copy ratio_graph in
  let edges = G.fold_edges_e (fun edge  l -> edge::l) target_ratio_graph [] in
  let edge_to_resample = List.nth edges 1 in
  let (_, (p1, r, p2), _) = edge_to_resample in
  r := 0.5;

  let schedule = get_schedule ratio_graph in
  exhaustive_heuristic ratio_graph schedule 1;

  assert_equal  ~printer:format_graph ~cmp:Downsampling.G.equal target_ratio_graph ratio_graph

  (* Should fail for now... *)
let downsampling test_ctxt =
  let open Flowgraph in
  let graph, deadline,resamplerDuration = Audiograph_parser_tests.parse_file "tests/downsampling_test.ag" in
  assert_equal (G.nb_vertex graph) 4;
  let durations node  =
    let label = G.V.label node in
    label.wcet |? 0.
  in
  let target_graph = G.copy graph in
  let node_before_resampler = Option.get (G.fold_vertex (fun vertex found -> match found with None when (G.V.label vertex).id = "n1" -> Some vertex | None -> None | _ -> found) target_graph None ) in
  let succ_node = List.hd (G.succ target_graph node_before_resampler) in
  G.remove_edge target_graph node_before_resampler succ_node;
  let resampler_node = G.V.create (Downsampling.make_resampler_node "res1"  1 0.5) in
  G.add_edge_e target_graph (G.E.create node_before_resampler (1,1) resampler_node);
  G.add_edge_e target_graph (G.E.create resampler_node (1,1) succ_node);

  Downsampling.dowsample_components graph durations resamplerDuration (Option.get deadline);

  assert_equal ~printer:G.format_graph ~cmp:equal_content target_graph graph


let suite = "downsampling" >::: ["graph_to_ratio_graph" >:: graph_to_ratio_graph;
                                 "ratio_graph_to_graph" >:: ratio_graph_to_graph;
                                 "merge_resamplers_before" >:: merge_resamplers_before;
                                 "merge_resamplers_after" >:: merge_resamplers_after;
                                 "exhaustive_heuristic" >:: exhaustive_heuristic;
                                 "downsampling" >:: downsampling]
