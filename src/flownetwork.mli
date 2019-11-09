open Graph

type flow = int
type capacity = int

type edge = flow * capacity

type flownetwork = edge graph

val ford_fulkerson: flownetwork -> id -> id -> flownetwork

