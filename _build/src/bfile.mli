(*open Flownetwork*)
open Weightedflownetwork

type path = string

(*val from_file: path -> flownetwork*)
val from_file: path -> weightedflownetwork

(*val export: flownetwork -> path -> path -> unit*)
val export: weightedflownetwork -> path -> path -> string -> unit

