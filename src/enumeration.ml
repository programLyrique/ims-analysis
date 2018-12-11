(** Enumerating all possible degraded versions of an audio graph *)


(*Enumerate ordered pairs among an array A^n_2, apply a function f on the pairs and collect the results *)
let map_enumerate_pairs tab f =
  let res = ref [];
  for i = 0 to Array.length tab - 1 do
    for j = i +1  to Array.length tab - 1 do
      res := (f tab.(i) tab.(j)::!res;
      res := (f tab.(j) tab.(i)::!res;
    done;
  done

(**List all directed acyclic graphs with n nodes/vertices and all vertices are connected *)
let dags n =
  (* Create n vertices *)

  (*Enumerate all possible edges. *)
  let rec gen_edges current_graph active_nodes =
    match active_nodes with
    | [] -> current_graph
    |  s::l -> gen_edges

  in
