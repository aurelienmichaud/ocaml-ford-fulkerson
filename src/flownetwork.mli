open Graph

type flow = int
type capacity = int

type edge = flow * capacity

type flownetwork = edge graph

val ford_fulkerson: flownetwork -> id -> id -> flownetwork
(* Alias for ford_fulkerson *)
val max_flow: flownetwork -> id -> id -> flownetwork

