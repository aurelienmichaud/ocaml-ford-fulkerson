(*open Flownetwork*)
open Weighedflownetwork

(* bg : bipartite graph
 * b:*)
let solve bg = Weighedflownetwork.ford_fulkerson bg 9998 9999

