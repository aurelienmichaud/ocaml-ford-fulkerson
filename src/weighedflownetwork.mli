open Graph

type flow       = int
type capacity   = int
type weight     = int

type edge = flow * capacity * weight

type weighedflownetwork = edge graph

val busacker_gowen: weighedflownetwork -> id -> id -> weighedflownetwork

