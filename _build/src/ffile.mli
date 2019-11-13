open Flownetwork

type path = string

val from_file: path -> flownetwork
val from_file_to_bipartite: path -> flownetwork

val write_file: path -> flownetwork -> unit

val export: path -> flownetwork -> unit

val export_same_shape: path -> flownetwork -> flownetwork -> unit

