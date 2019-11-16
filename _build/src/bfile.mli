(*open Flownetwork*)
open Weighedflownetwork

type path = string

(*val from_file: path -> flownetwork*)
val from_file: path -> weighedflownetwork

(*val export: flownetwork -> path -> path -> unit*)
val export: weighedflownetwork -> path -> path -> string -> unit

