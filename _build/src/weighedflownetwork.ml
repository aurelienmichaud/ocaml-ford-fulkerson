open Graph
open Tools

type flow       = int
type capacity   = int
type weight     = int

type edge = flow * capacity * weight

type weighedflownetwork = edge graph


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
type residual_graph = (flow * weight * direction) graph

exception Found_Augmenting_Path of (((id * direction) list) * int)

let get_residual_graph fn = 
    let cloned = clone_nodes fn in
    e_fold
    fn
    (fun residual_graph id1 id2 (flow, capacity, weight) ->
        if capacity <> 0 then
        begin
            if flow <> 0 && flow <> capacity then
                let new_residual_graph = new_arc residual_graph id1 id2 (capacity - flow, weight, Same) in
                new_arc new_residual_graph id2 id1 (flow, -weight, Opposite) 
            else if flow = 0 then
                new_arc residual_graph id1 id2 (capacity, weight, Same)
            (* flow = capacity *)
            else
                new_arc residual_graph id2 id1 (capacity, -weight, Opposite)
        end
        else
            residual_graph)
    cloned

let bellman_ford rg sc sk =

    let hashtbl = Hashtbl.create 100 in
    (* Insert the source node in the hash table with a weight of 0 and
     * itself as its predecessor to start the algorithm *)
    Hashtbl.add hashtbl sc (0, sc);
    (* Insert the sink node with maximum weight so that it will be replaced later *)
    Hashtbl.add hashtbl sk (999999, sk);

    let rec _bellman_ford visiting_node_id visited = function
        | []    -> visiting_node_id :: visited
        | (id, (residual_flow, weight, direction)) :: t ->

            let (predecessor_weight, _) = Hashtbl.find hashtbl visiting_node_id in
        
            let has_been_modified =
                begin
                    try
                        let (node_weight, predecessor) = Hashtbl.find hashtbl id in
                        
                        if node_weight > predecessor_weight + weight then
                            let _ = Hashtbl.replace hashtbl id (predecessor_weight + weight, visiting_node_id) in
                            true
                        else false
            
                    with Not_found ->
                            Hashtbl.add hashtbl id (predecessor_weight + weight, visiting_node_id);
                            true
                end
            in

            (*if not (List.mem id visited) then*)
            let new_visited =
                    _bellman_ford visiting_node_id (id::visited) t
            in
            (* If the id has not already been visited (its out_arcs have been parsed) 
             * OR if he has been visited but we just modified its weight, 
             * we need to parse its out_arcs again *)
            if not (List.mem id visited) || has_been_modified then 
                _bellman_ford id new_visited (out_arcs rg id)
            else new_visited
    in


    (* Get path by parsing back hashtable *)
    let rec get_path_from_hashtbl acc_path current_id delta = 
        if current_id = sc then
            (* This direction is not important, it won't be read by update_flow_network function *)
            ((sc, Same)::acc_path, delta)
        else
            let (_, predecessor_id) = Hashtbl.find hashtbl current_id in

            match find_arc rg predecessor_id current_id with
                | Some (residual_flow, _, direction) -> 
                    let delta = if residual_flow < delta || delta = 0 then residual_flow else delta in
        
                    get_path_from_hashtbl ((current_id, direction) :: acc_path) predecessor_id delta

                (* Not supposed to get there *)
                | None -> failwith "bellmand_ford";
    in
    
    (* Bellman-Ford final table *)
    let visited = _bellman_ford sc [sc] (out_arcs rg sc) in

    if List.mem sk visited then
        (* We start the search from the sink node *)
        let (path, delta) = get_path_from_hashtbl [] sk 0 in

        raise (Found_Augmenting_Path (path, delta))
    (* In that case, the sink node has not been visited by bellman-ford. Hence we did
     * not find an augmenting path. So we do not raise a Found_Augmenting_Path *)
    else
        visited        
    

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
    bellman_ford rg sc sk


let update_edge fn id1 id2 delta =
    match find_arc fn id1 id2 with 
    | None  -> fn
    | Some (flow, capacity, weight) -> new_arc fn id1 id2 (flow + delta, capacity, weight)

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


(* Busacker-Gowen Algorithm on weighed flow networks

 * fn : flownetwork
 * sc: Source node, the node we begin our research from
 * sk: Sink node, the node we are looking for

 * Principle of the algorithm :
 
 * While we can find an augmenting path
 *      - Find an augmenting path 
 *      - Get the bottleneck capacity (basically the delta by which we can increase the flow in the augmenting path) 
 *      - Update the flow network with that bottleneck capacity in the augmenting path

 *)
let busacker_gowen wfn sc sk = 
    (* Check whether the start node or the end node does not exist *)
    if (not (node_exists wfn sc)) || (not (node_exists wfn sk)) then
        raise (Graph_error ("Source node and/or Sink node do/es not exist in flow network"))
    else
    
    let rec loop wfn = 
        (* Get the residual graph from the flow network *)    
        let residual_fn = get_residual_graph wfn in

        try
            (* Find an augmenting path in the residual graph *)
            let _ = find_augmenting_path residual_fn sc sk in
            (* If no exception is raised, then we finished the algorithm *)
            wfn
        (* If we found such a path, update the flow network with the bottleneck capacity delta
         * and do it all again *)
        with (Found_Augmenting_Path (path, delta))  -> loop (update_flow_network wfn delta path)
    in
    loop wfn

let max_flow_min_cost wfn sc sk = busacker_gowen wfn sc sk




