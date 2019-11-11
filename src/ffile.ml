open Tools

type path = string

let write_file path flownet = 
    let string_flownet = gmap flownet (fun (f, c) -> (string_of_int f) ^ "/" ^ (string_of_int c)) in
    Gfile.write_file path string_flownet

(* Only use this if the graph file already contains
 * flow/capacity, and not just the capacity *)
let edge_of_string str = 
    match String.split_on_char '/' str with
    | [f;c] -> (int_of_string f, int_of_string c)
    | _     -> (0,0) (* Not supposed to get there *)

let from_file path =
    let string_flownet = Gfile.from_file path in
    gmap string_flownet (fun s -> 0, (int_of_string s))

let export path flownet = 
    let string_flownet = gmap flownet (fun (f, c) -> (string_of_int f) ^ "/" ^ (string_of_int c)) in
    Gfile.export path string_flownet

let export_same_shape path flownet_from flownet_to =
    let string_flownet_from = gmap flownet_from (fun (f, c) -> (string_of_int f) ^ "/" ^ (string_of_int c)) in
    let string_flownet_to = gmap flownet_to (fun (f, c) -> (string_of_int f) ^ "/" ^ (string_of_int c)) in
    Gfile.export_same_shape path string_flownet_from string_flownet_to
  

