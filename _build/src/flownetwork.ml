open Graph
open Tools

type flow = int
type capacity = int

type edge = flow * capacity

type flownetwork = edge graph


(* [WIP] Update arc between id1 & id2 nodes
in flownetwork "fn" with flow value "flow".
 * fn : the flownetwork we are searching in.
 * id1 : start node.
 * id2 : end node.
 * flow : flow update. 
 update : flownetwork -> id -> id -> int -> ()
 TODO : Improve : both arcs with 1 function 
let update fn id1 id2 flow =

  let () =
    (* Updating arc, if it exists *)
    match find_arc fn id1 id2 with
      | None -> ()
      | Some arc -> add_arc fn id1 id2 flow
    (* Second arc
    match find_arc fn id2 id1 with
      | None -> ()
      | Some arc -> add_arc fn id2 id1 (-flow)) *)
  in
  ()
            
*)

type direction = Same | Opposite
type residual_graph = (flow * direction) graph

exception Found_Augmenting_Path of (((id * direction) list) * int)

let get_residual_graph fn = 
    let cloned = clone_nodes fn in
    e_fold
    fn
    (fun residual_graph id1 id2 (flow, capacity) ->
        if capacity <> 0 then
        begin
            if flow <> 0 && flow <> capacity then
                let new_residual_graph = new_arc residual_graph id1 id2 (capacity - flow, Same) in
                new_arc new_residual_graph id2 id1 (flow, Opposite) 
            else if flow = 0 then
                new_arc residual_graph id1 id2 (capacity, Same)
            else
                new_arc residual_graph id2 id1 (capacity, Opposite)
        end
        else
            residual_graph)
    cloned

(* === Depth First Search === *)
let dfs rg sc sk = 
    let rec _dfs acc delta visited = function
        | []            -> visited
        | (id, (residual_flow, direction))::t  -> 
            (* We found the node we were looking for, so we stop the loop by raising an exception *)
            if id = sk then
                let delta = if residual_flow < delta || delta = 0 then residual_flow else delta in
                raise (Found_Augmenting_Path (List.rev ((sk, direction)::acc), delta))

            (* We already visited that node, so we ignore it and parse the other out_arcs provided in the list *)
            else if List.mem id visited then
                _dfs acc delta visited t

            (* We need to parse the node id first, since we use Depth First Search (DFS) *)
            else
                let new_visited =
                    if residual_flow < delta || delta = 0 then
                        _dfs ((id, direction)::acc) residual_flow (id::visited) (out_arcs rg id)
                    else
                        _dfs ((id, direction)::acc) delta (id::visited) (out_arcs rg id)
                in
                _dfs acc delta new_visited t
    in
    _dfs [(sc, Same)] 0 [sc] (out_arcs rg sc)



(* === Breadth First Search === *)
let bfs rg sc sk =
    let rec _bfs acc delta visited = function
        | []    -> visited
        | (id, (residual_flow, direction))::t ->
            if id = sk then
                let delta = if residual_flow < delta || delta = 0 then residual_flow else delta in
                raise (Found_Augmenting_Path (List.rev ((sk, direction)::acc), delta))

            (* We already visited that node, so we ignore it and parse the other out_arcs provided in the list *)
            else if List.mem id visited then
                _bfs acc delta visited t

            (* We need to parse the rest 't' of the list, since we use Breadth First Search (BFS) *)
            else
                let new_visited =
                    _bfs acc delta (id::visited) t
                in
                if residual_flow < delta || delta = 0 then
                    _bfs ((id, direction)::acc) residual_flow new_visited (out_arcs rg id)
                else
                    _bfs ((id, direction)::acc) delta new_visited (out_arcs rg id)

    in
    _bfs [(sc, Same)] 0 [sc] (out_arcs rg sc)


(* Find one augmenting path in a residual graph
 * It returns the found path, and the bottleneck capacity (delta)
 * by which we will increase the flow of each arc described in the path
 * The returning path is shaped as follow :
 * path : (id, direction) list
 * For instance : path = [(sc, Same); (3, Opposite); (7, Same); (sk, Same)]
 * which means we will need (in ford-fulkerson algorithm) to update 
 * the following arcs :
 * (sc, 3) with +delta; (3, 7) with -delta; (7, sk) with +delta
 * So 'Same direction means we will increase by +delta, 
 * and 'Opposite' direction by -delta.
 * The direction of the first pair (id, direction) doesn't matter

 * Keep in mind that with floating capacities and flows, it will only terminate
 * with Breadth First Search (BFS) algorithm. But, for now, since we only use
 * integer operations, we assume we are only working on integers.
 *)
let find_augmenting_path rg sc sk = 
    (* Either dfs, bfs.
     * They all respect the same output format for the resulting path *)
    bfs rg sc sk


let update_edge fn id1 id2 delta =
    match find_arc fn id1 id2 with 
    | None  -> fn
    | Some (flow, capacity) -> new_arc fn id1 id2 (flow + delta, capacity)

(* This function receives the path '(id, direction) list'
 * resulting from 'find_augmenting_path' *)
let rec update_flow_network fn delta = function
    | []                -> fn
    | [(id, direction)] -> fn
    (* We cannot use add_arc, since it takes an int graph, and
     * a flow network is a (flow * capacity) graph *)
    | (id1, dir1)::((id2, dir2)::t as tail)   ->    
        match dir2 with
        | Same      -> update_flow_network (update_edge fn id1 id2 delta) delta tail
        | Opposite  -> update_flow_network (update_edge fn id1 id2 (-delta)) delta tail


(* Ford-Fulkerson Algorithm on flow networks

 * fn : flownetwork
 * sc: Source node, the node we begin our research from
 * sk: Sink node, the node we are looking for

 * Principle of the algorithm :
 
 * While we can find an augmenting path
 *      - Find an augmenting path 
 *      - Get the bottleneck capacity (basically the delta by which we can increase the flow in the augmenting path) 
 *      - Update the flow network with that bottleneck capacity in the augmenting path

 *)
let ford_fulkerson fn sc sk = 
    (* Check whether the start node or the end node does not exist *)
    if (not (node_exists fn sc)) || (not (node_exists fn sk)) then
        raise (Graph_error ("Source node and/or Sink node do/es not exist in flow network"))
    else
    
    let rec loop fn = 
        (* Get the residual graph from the flow network *)    
        let residual_fn = get_residual_graph fn in

        try
            (* Find an augmenting path in the residual graph *)
            let _ = find_augmenting_path residual_fn sc sk in
            (* If no exception is raised, then we finished the algorithm *)
            fn
        (* If we found such a path, update the flow network with the bottleneck capacity delta
         * and do it all again *)
        with (Found_Augmenting_Path (path, delta))  -> loop (update_flow_network fn delta path)
    in
    loop fn

    
let max_flow fn sc sk = ford_fulkerson fn sc sk




