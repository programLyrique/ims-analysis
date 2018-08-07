open OUnit2
open Batteries


let graph_to_ratio_graph test_ctxt =
  let open Flowgraph in
  let node1 = Flowgraph.G.V.create {id="id-1"; nb_inlets=1; nb_outlets=1; className="plop"; text=None ; more=[] } in
  let node2 = Flowgraph.G.V.create {id="id-2"; nb_inlets=1; nb_outlets=1; className="plop"; text=None ; more=[] } in
  let node3 = Flowgraph.G.V.create {id="id-3"; nb_inlets=2; nb_outlets=1; className="mix"; text=None ; more=[] } in
  let edge1 = Flowgraph.G.E.create node1 (1,1) node3 in
  let edge2 = Flowgraph.G.E.create node2 (1,2) node3 in
  let graph = Flowgraph.G.create ~size:3 () in
  Flowgraph.G.add_edge_e graph edge1;
  Flowgraph.G.add_edge_e graph edge2;
  assert_equal (G.nb_vertex graph) 3;
  assert_equal (G.nb_edges graph) 2;
  let ratio_graph = Downsampling.graph_to_ratio_graph graph in
  let ratio_graph_c = Downsampling.G.create ~size:3 () in
  let edge1_r = Downsampling.G.E.create (Downsampling.G.V.create (node1, ref false)) (1, ref 1., 1) (Downsampling.G.V.create (node3, ref false)) in
  let edge2_r = Downsampling.G.E.create (Downsampling.G.V.create (node2, ref false)) (1, ref 1., 2) (Downsampling.G.V.create (node3, ref false)) in
  ignore (Downsampling.G.add_edge_e ratio_graph_c edge1_r);
  ignore (Downsampling.G.add_edge_e ratio_graph_c edge2_r);
  assert_equal (Downsampling.G.nb_vertex ratio_graph) 3;
  assert_equal (Downsampling.G.nb_edges ratio_graph) 2;
  let format_edge  (((n1, c1), (i, r, o), (n2, c2)) : Downsampling.G.E.t) = Printf.sprintf "((%s, %b), (%d, %f, %d), (%s, %b))\n" (Flowgraph.show_node (G.V.label n1)) !c1 i !r o (Flowgraph.show_node (G.V.label n2)) !c2 in
  let format_graph graph = Downsampling.G.fold_edges_e (fun edge s -> Printf.sprintf "%s%s" s (format_edge edge)) graph "" in
  assert_equal  ~printer:format_graph ~cmp:Downsampling.G.equal ratio_graph ratio_graph_c


let ratio_graph_to_graph test_ctxt =
  let open Flowgraph in
  let node1 = Flowgraph.G.V.create {id="id-1"; nb_inlets=1; nb_outlets=1; className="plop"; text=None ; more=[] } in
  let node2 = Flowgraph.G.V.create {id="id-2"; nb_inlets=1; nb_outlets=1; className="plop"; text=None ; more=[] } in
  let node3 = Flowgraph.G.V.create {id="id-3"; nb_inlets=2; nb_outlets=1; className="mix"; text=None ; more=[] } in
  let edge1 = Flowgraph.G.E.create node1 (1,1) node3 in
  let edge2 = Flowgraph.G.E.create node2 (1,2) node3 in
  let graph = Flowgraph.G.create ~size:3 () in
  Flowgraph.G.add_edge_e graph edge1;
  Flowgraph.G.add_edge_e graph edge2;
  assert_equal (G.nb_vertex graph) 3;
  assert_equal (G.nb_edges graph) 2;
  let ratio_graph = Downsampling.G.create ~size:3 () in
  let node3_r = Downsampling.G.V.create (node3, ref true) in
  let edge1_r = Downsampling.G.E.create (Downsampling.G.V.create (node1, ref false)) (1, ref 0.5, 1) node3_r in
  let edge2_r = Downsampling.G.E.create (Downsampling.G.V.create (node2, ref false)) (1, ref 0.5, 2) node3_r in
  ignore (Downsampling.G.add_edge_e ratio_graph edge1_r);
  ignore (Downsampling.G.add_edge_e ratio_graph edge2_r);
  let graph_c = G.copy graph in (* Will give new tags for vertices. So we need to use the original one or hack a bit on ctn_vertex. *)
  Downsampling.ratio_graph_to_graph ratio_graph graph;
  G.clear graph_c;
  let unique_id =
    let id  = ref 0 in
      function  () -> incr id; !id in
  (* Adding manually resampler to graph to compare with graph_c *)
  let resampler2 = G.V.create {id="res" ^(string_of_int (unique_id ())); nb_inlets=1; nb_outlets=1; className="resampler"; text=None ; more=[("ratio", "0.5")] } in
  let resampler1 = G.V.create {id="res" ^(string_of_int (unique_id ())); nb_inlets=1; nb_outlets=1; className="resampler"; text=None ; more=[("ratio", "0.5")] } in
  let e1 = G.E.create node1 (1,1) resampler1 and e2 = G.E.create node2 (1,1) resampler2 in
  let e1' = G.E.create resampler1 (1,1) node3 and e2' = G.E.create resampler2 (1,2) node3 in
  G.add_edge_e graph_c e1 ; G.add_edge_e graph_c e2;G.add_edge_e graph_c e1'; G.add_edge_e graph_c e2';

  assert_equal  ~printer:G.format_graph ~cmp:equal graph graph_c



let suite = "downsampling" >::: ["graph_to_ratio_graph" >:: graph_to_ratio_graph;
                                 "ratio_graph_to_graph" >:: ratio_graph_to_graph]