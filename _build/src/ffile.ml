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




(*
 * id 998 and 999 are forbidden since they are used for source and sink nodes
 * File format :
 * 2=4,5,6 
 * 2=8
 * 1,2,3=7,9
*)

let rec create_nodes graph = function
    | []    -> graph
    | h::t  -> 
        try
            let ng = new_node graph (int_of_string h) in
            create_nodes ng t
        with (Graph_error (e))  -> create_nodes graph t

let rec create_arcs graph from = function
    | []    -> graph
    (* (flow, capacity) = (0, 1) for bipartite graph *)
    | h::t  ->
        let ng = new_arc graph (int_of_string from) (int_of_string h) (0, 1)
        in
        create_arcs ng from t

let read_line graph line =
    try 
        let parse = String.split_on_char '=' line in 

        let (_froms, _tos) =
            match parse with
            | [f;t] -> (f,t)
            | _   -> raise Not_found
        in

        let froms   = String.split_on_char ',' _froms in
        let tos     = String.split_on_char ',' _tos in

        let graph = create_nodes graph tos in

        let graph = create_nodes graph froms in 

        let graph = 
            List.fold_left
            (fun gr h -> 
                create_arcs gr h tos)
            graph
            froms
        in

        (* Link the source node to all froms *)
        let graph = 
            List.fold_left
            (fun gr h ->
                new_arc gr 998 (int_of_string h) (0, 1))
            graph
            froms
        in

        (* Link all tos to the sink node *)
        let graph = 
            List.fold_left
            (fun gr h ->
                new_arc gr (int_of_string h) 999 (0, 1))
            graph
            tos
        in
        graph

    with _  ->
        Printf.printf "Unknown line:\n%s\n%!" line;
        failwith "from_file_to_bipartite"

let from_file_to_bipartite path =

    let infile = open_in path in

    let rec loop graph = 
        try
            let line = input_line infile in

            let line = String.trim line in

            let graph2 = 

                if line = "" then graph

                else match line.[0] with 
                    | '%'   -> graph
                    | _     -> read_line graph line
            in 
            loop graph2

        with End_of_file    -> graph (* Done *)
  
    in

    (* 998 -> source node
     * 999 -> sink node *)
    let base_graph = new_node empty_graph 998 in
    let base_graph = new_node base_graph 999 in

    let final_graph = loop base_graph in

    close_in infile;
    final_graph

