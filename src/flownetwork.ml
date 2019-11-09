open Graph

type flow = int
type capacity = int

type edge = flow * capacity

type flownetwork = edge graph

(* Depth First Search algorithm
 * s: Start node, the node we begin our research
 * e: End node, the node we are looking for. Once we reached it, we end the search
 *)
let dfs s e = 
   () 

(* fn : flownetwork *)
(* s: Start node, the node we begin our research
 * e: End node, the node we are looking for
 *)

let ford_fulkerson fn s e = 
    (* Check whether the start node or the end node does not exist *)
    if (not (node_exists fn s)) || (not (node_exists fn e)) then
        raise (Graph_error ("Start node or End node do not exist in flow network"))
    else


    fn

