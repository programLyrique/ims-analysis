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
  let ratio_graph = Downsampling.graph_to_ratio_graph graph in
  let ratio_graph_c = Downsampling.G.create ~size:3 () in
  let edge1_r = Downsampling.G.E.create (Downsampling.G.V.create (node1, ref false)) (1, ref 1., 1) (Downsampling.G.V.create (node3, ref false)) in
  let edge2_r = Downsampling.G.E.create (Downsampling.G.V.create (node2, ref false)) (1, ref 1., 2) (Downsampling.G.V.create (node3, ref false)) in
  ignore (Downsampling.G.add_edge_e ratio_graph_c edge1_r);
  ignore (Downsampling.G.add_edge_e ratio_graph_c edge2_r);
  let format_edge  (((n1, c1), (i, r, o), (n2, c2)) : Downsampling.G.E.t) = Printf.sprintf "((%s, %b), (%d, %f, %d), (%s, %b))\n" (Flowgraph.show_node (G.V.label n1)) !c1 i !r o (Flowgraph.show_node (G.V.label n2)) !c2 in
  let format_graph graph = Downsampling.G.fold_edges_e (fun edge s -> Printf.sprintf "%s%s" s (format_edge edge)) graph "" in

  assert_equal  ~printer:format_graph ~cmp:Downsampling.G.equal ratio_graph ratio_graph_c






let suite = "downsampling" >::: ["graph_to_ratio_graph" >:: graph_to_ratio_graph]
