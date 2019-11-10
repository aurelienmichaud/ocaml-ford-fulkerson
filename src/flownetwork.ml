open Graph

type flow = int
type capacity = int

type edge = flow * capacity

type flownetwork = edge graph

(* Get all the outcoming nodes of node 'id', 
 * with the following condition : flow < capacity *)
let get_out_nodes fn id = 
    List.fold_left 
        (fun acc (_id, (_flow, _capacity)) -> if _flow < _capacity then _id::acc else acc) 
        []
        (out_arcs fn id)

(* Get all the outcoming arcs of node 'id', 
 * with the following condition : flow < capacity *)
let get_out_arcs fn id =
    List.fold_left 
        (fun acc ((_id, (_flow, _capacity)) as arc) -> if _flow < _capacity then arc::acc else acc) 
        []
        (out_arcs fn id)

(* Get all the outcoming arcs of node 'id', 
 * with the following condition : flow < capacity
 * but we only return the id of the node AND the gap
 * capacity - flow *)
let get_out_arcs_gaps fn id =
    List.fold_left 
        (fun acc (_id, (_flow, _capacity)) -> if _flow < _capacity then (_id, _capacity - _flow)::acc else acc) 
        []
        (out_arcs fn id)







(* Exception raised when in find_augmenting_path we found the ending node
 * which means we found an augmenting path.
 * We use an exception in order to stop the entire search algorithm *)
exception Found_Augmenting_Path of ((id list) * int)

(* Find one augmenting path,
 * i.e. a path in graph fn from node 's' to node 'e'
 * where all the edges verify the following condition :
 * flow < capacity *)
let find_augmenting_path fn s e = 
    (* Depth First Search *)
    let rec dfs acc delta visited = function
    | []            -> visited
    | (id, gap)::t  -> 
        (* We found the node we were looking for, so we stop the loop by raising an exception *)
        if id = e then
            let delta = if gap < delta || delta = 0 then gap else delta in
            raise (Found_Augmenting_Path (List.rev (e::acc), delta))

        (* We already visited that node, so we ignore it and parse the other out_arcs provided in the list *)
        else if List.mem id visited then
            dfs acc delta visited t

        (* We need to parse the node id first, since we use Depth First Search (DFS) *)
        else
            let new_visited = 
                if gap < delta || delta = 0 then
                    dfs (id::acc) gap (id::visited) (get_out_arcs_gaps fn id)
                else
                    dfs (id::acc) delta (id::visited) (get_out_arcs_gaps fn id)
            in
            dfs acc delta new_visited t
        
    in
    dfs [s] 0 [s] (get_out_arcs_gaps fn s)


let update_edge fn id1 id2 delta =
    match find_arc fn id1 id2 with 
    | None  -> fn
    | Some (flow, capacity) -> new_arc fn id1 id2 (flow + delta, capacity)

let rec update_flow_network fn delta = function
    | []            -> fn
    | [id]          -> fn
    (* We cannot use add_arc, since it takes an int graph, and
     * a flow network is a (flow * capacity) graph *)
    | id1::(id2::t as tail)   -> update_flow_network (update_edge fn id1 id2 delta) delta tail



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




(* fn : flownetwork *)
(* s: Start node, the node we begin our research from
 * e: End node, the node we are looking for
 *)

let ford_fulkerson fn s e = 
    (* Check whether the start node or the end node does not exist *)
    if (not (node_exists fn s)) || (not (node_exists fn e)) then
        raise (Graph_error ("Start node and/or End node do/es not exist in flow network"))
    else
    
    (* TEST 
     * Find one augmenting path, update the flow network with the delta found
     * in that augmented path, and return that new flow network *)
    try
        let _ = find_augmenting_path fn s e in
        fn
    with
        (Found_Augmenting_Path (path, delta))   -> update_flow_network fn delta path


