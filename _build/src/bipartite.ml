open Weightedflownetwork

(* bg : bipartite graph, basically a weighed flow network *)
let solve bg = Weightedflownetwork.max_flow_min_cost bg 9998 9999

