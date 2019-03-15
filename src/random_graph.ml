(** Randomly generates DAGs *)
open Batteries
open Flowgraph
open Graph

(* See https://stackoverflow.com/questions/12790337/generating-a-random-dag
   Generate a Dag goven a number of edges and a number of vertices *)
let gen_dag_v_e nb_vertices nb_edges =
  let n = nb_vertices * (nb_vertices - 1) / 2 in
  assert (0 < nb_edges);
  assert (nb_edges <= n);
  let sample = Array.init nb_edges (fun i -> Random.int (n - nb_edges + 1))  in
  Array.sort compare sample;
  (*Make unique *)
  Array.modifyi (fun i a -> a + i ) sample;
  assert (Array.for_all (fun a -> a >=0 && a <= n - 1) sample);

  let endpoints = Enum.range ~until:(nb_vertices - 1) 0 in
  let endpoints = Random.shuffle endpoints in

  let vertices = Array.init nb_vertices (fun i -> G.V.create (Node.make ("id-" ^ string_of_int i ) 1 1 "plop")) in
  let graph = G.create ~size:nb_vertices () in
  Array.iter (fun v -> G.add_vertex graph v) vertices;
  Array.iter (fun v ->
      let tail = int_of_float (0.5 +. sqrt((float_of_int v +. 1.) *. 2.)) in
      let head = v - tail * (tail - 1) / 2 in
      let src = endpoints.(tail) in
      let dst = endpoints.(head) in
      G.add_edge_e graph (G.E.create vertices.(src) (1,1) vertices.(dst))
    ) sample;
  graph

(* Generate a Dag given a number of nodes and a probability of having an edge between two vertices *)
let  gen_dag_v nb_vertices p =
  (*So that we don't have the same ids... Do we actually really care as node generation will be done later? *)
  let endpoints = Enum.range ~until:(nb_vertices - 1) 0 in
  let endpoints = Random.shuffle endpoints in
  (*Get the actual nodes*)
  let vertices = Array.init nb_vertices (fun i -> G.V.create (Node.make ("id-" ^ string_of_int i ) 1 1 "plop")) in
  let graph = G.create ~size:nb_vertices () in
  Array.iter (fun v -> G.add_vertex graph v) vertices;
  (*Connect all the vertices of endpoints to ensure weak connectiveness*)
  (*for i=0 to nb_vertices - 2 do
    let src = vertices.(endpoints.(i)) in
    let dst = vertices.(endpoints.(i+1)) in
    G.add_edge_e graph (G.E.create src (1,1) dst)
    done;*)
  let one = 1. +. epsilon_float in (*We want an uniform distribution in the close range [0,1]*)
  for i = 0 to nb_vertices - 1 do
    for j = 0 to i - 1 do
      if Random.float one <= p then
        let src = vertices.(endpoints.(i)) in
        let dst = vertices.(endpoints.(j)) in
        if not (G.mem_edge graph src dst) then
          G.add_edge_e graph (G.E.create src (1,1) dst)
    done
  done;
  graph
(* Let's remove nodes without edges and return the result. But we connected all nodes using the endpoints shuffling *)
  (*NodeMapper.filter_map (fun v -> if G.in_degree graph v = 0 && G.out_degree graph v = 0 then None else Some v) graph*)


module UndirectedG = struct
  include Imperative.Graph.AbstractLabeled(Node)(Edge)
  include Builder.I(Imperative.Graph.AbstractLabeled(Node)(Edge))
end
module ToUndirected = struct
  include Gmap.Edge(G)(UndirectedG)
  let build_table graph =
    let hashtbl = Hashtbl.create (G.nb_vertex graph) in
    G.iter_vertex (fun v -> Hashtbl.add hashtbl v (UndirectedG.V.create (G.V.label v))) graph;
    hashtbl
  let to_undirected_edge hashtbl e = UndirectedG.E.create (Hashtbl.find hashtbl (G.E.src e))  (G.E.label e) (Hashtbl.find hashtbl (G.E.dst e))
  let to_undirected graph =
    let hashtbl = build_table graph in
    map (to_undirected_edge hashtbl) graph
  (* Also keeps vertices that did not have edges to or from *)
  let to_undirected_preserve graph =
    let hashtbl = build_table graph in
    let g = map (to_undirected_edge hashtbl) graph in
    (*We now need to add isolated edges*)
    G.iter_vertex (fun v -> if G.out_degree graph v = 0 && G.in_degree graph v = 0 then ignore (UndirectedG.add_vertex g (Hashtbl.find hashtbl v))) graph;
    g
end
module Chooser = Oper.Choose(UndirectedG)
module Traversal = Traverse.Dfs(UndirectedG)
module MaxComponents = Components.Undirected(UndirectedG)



(*Returns the biggest component *)
let max_component graph =
  let ugraph = ToUndirected.to_undirected graph in (*Will get rid of orphan nodes, which is good.*)
  assert (G.nb_vertex graph >= UndirectedG.nb_vertex ugraph);
  (*Printf.printf "edges: before=%d; after=%d\n" (G.nb_edges graph) (UndirectedG.nb_edges ugraph);*)
  assert (G.nb_edges graph = UndirectedG.nb_edges ugraph);
  let components = MaxComponents.components_array ugraph in
  (*Printf.printf "There are %d components\n" (Array.length components);*)
  let _,max_component_i = Array.fold_lefti (fun a i g -> let (m, i_m) = a in
                                         let size = List.length g in
                                           if size > m then (m, i) else a ) (0,-1) components in
  let max_component = List.map UndirectedG.V.label components.(max_component_i) in
  EdgeMapper.filter_map (fun e -> let src = G.V.label (G.E.src e) in
                          let dst = G.V.label (G.E.dst e) in
                      if List.mem src max_component && List.mem dst max_component then Some e else None) graph



(*Check if a graph is (weakly) connected. *)
let connected graph =
  assert (not (G.is_empty graph));
  let ugraph = ToUndirected.to_undirected graph in
  (*map on edges does not keep nodes without edges going to or coming from it. So the original graph is connected only if the original graph had only one node.*)
  if UndirectedG.is_empty ugraph then G.nb_vertex graph = 1
  else
    let v = Chooser.choose_vertex ugraph in
    let nb_nodes = Traversal.fold_component (fun v n -> n + 1) 0 ugraph v in
    (*Printf.printf "There are %d nodes here\n" nb_nodes;*)
    UndirectedG.nb_vertex ugraph = nb_nodes

(* Generate nb_samples graphs with each being nb_vertices where adges have probability p *)
let gen_random_dags nb_vertices p nb_samples =
  (* m is used to stop generation if it takes too much time *)
  (*let n = float_of_int nb_vertices in
  let nb_edges = int_of_float (p *. n *. (n -. 1.) /. 2.) in
    Printf.printf "Generating with %d nodes and approximately %d edges\n" nb_vertices nb_edges;*)
  let rec add_weakly_connected_dag n m =
    if n > 0 && m > 0 then
      begin
        let graph = max_component (gen_dag_v nb_vertices p) in
        (*Printf.printf "Graph has size: %d with %d edges\n" (G.nb_vertex graph) (G.nb_edges graph);*)
        assert (not (G.is_empty graph));
        assert (connected graph);
        graph::(add_weakly_connected_dag (n - 1) (m - 1 ))
      end
    else []
  in
  let m = 1000 * int_of_float ( (float_of_int nb_samples) *. 1. /. p ) in
  add_weakly_connected_dag nb_samples m
