open Graph
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

(* The file format can be the same as the graph file format
 * i.e. the label is only one number, in which case it will represent 
 * the capacity and the flow will be set with a value of 0.
 * Otherwise the file format can have that same format but with
 * string labels as follow: "13/26". Such case needs to parse the string
 * to get the flow (13) and the capacity (26) out of it.
*)
let from_file path =

    let parse s =
        match String.split_on_char '/' s with
            | [flow;capacity]   -> ((int_of_string flow), (int_of_string capacity))
            | _                 -> raise (Graph_error ("Incorrect file format"))
    in

    let string_flownet = Gfile.from_file path in
    (* Try the graph file format *)
    try
        gmap string_flownet (fun s -> 0, (int_of_string s))
    with _  ->
        (* Otherwise, try the flow/capacity string label format *)
        try
            gmap string_flownet (fun s -> parse s)
        with _  -> failwith "Error : Incorrect file format (Ffile.from_file function)"

let export path flownet = 
    let string_flownet = gmap flownet (fun (f, c) -> (string_of_int f) ^ "/" ^ (string_of_int c)) in
    Gfile.export path string_flownet

let export_same_shape path flownet_from flownet_to =
    let string_flownet_from = gmap flownet_from (fun (f, c) -> (string_of_int f) ^ "/" ^ (string_of_int c)) in
    let string_flownet_to = gmap flownet_to (fun (f, c) -> (string_of_int f) ^ "/" ^ (string_of_int c)) in
    Gfile.export_same_shape path string_flownet_from string_flownet_to



