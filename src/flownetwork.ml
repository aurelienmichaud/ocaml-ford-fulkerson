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

(* Find one augmenting path,
 * i.e. a path in graph fn from node 's' to node 'e'
 * where all the edges verify the following condition :
 * flow < capacity *)
let find_augmenting_path fn s e = ()

(* Depth First Search algorithm
 * fn: flownetwork we will be searching in
 * s: Start node, the node we begin our search from
 * e: End node, the node we are looking for. Once we reach it, we end the search
 *)
(* let dfs fn s e = 
    let rec _dfs acc visited_nodes = function 
    | []            -> (acc, visited_nodes) 
    | (id, lbl)::t  ->
        try
            let _id = List.find (fun i -> i = id) visited_nodes in
        with Not_found  ->
            _dfs id::acc id::visited_nodes 
*)

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


    fn

