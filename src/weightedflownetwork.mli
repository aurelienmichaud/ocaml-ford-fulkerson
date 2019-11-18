open Graph

type flow       = int
type capacity   = int
type weight     = int

type edge = flow * capacity * weight

type weightedflownetwork = edge graph

val busacker_gowen: weightedflownetwork -> id -> id -> weightedflownetwork

(* Alias for busacker_gowen *)
val max_flow_min_cost: weightedflownetwork -> id -> id -> weightedflownetwork 

