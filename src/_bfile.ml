
type path = string


let read_capacity_line n graph hashtbl name capacity = 

let read_connection_line n graph hashtbl weight froms tos = 

(* NameX[,NameY,...]=NameJ[,NameK,...]*)
let get_connection_line line =
    match String.split_on_char '=' line with
        | [froms;tos]   -> Some (froms, tos)
        | _             -> None

(* Weight|NameX[,NameY,...]=NameJ[,NameK,...]*)
let get_weighed_connection_line line =
    match String.split '|' line with
        | [weight;rest] -> 
            begin
            match get_connection_line rest with
                | Some (froms, tos) -> Some (weight, froms, tos)
                | _ -> None
            end
        | _ -> None


(* Name:Capacity, e.g. :
 * University1:20 *)
let get_capacity_line line = 
    match String.split_on_char ':' line with
        | [name;capacity]   -> Some (name, capacity)
        | _                 -> None

let read_line n graph hashtbl line =

    match get_capcity_line line with
        | Some (name, capacity) -> read_capacity_line n graph hashtbl name capacity
        | _ -> 

        match get_weighed_connection_line line with
            | Some (weight, froms, tos) -> read_connection_line n graph hashtbl weight froms tos
            | _ ->
    
            match get_connection_line line with
                | Some (froms, tos) -> read_connection_line n graph hashtbl 0 froms tos
                | _ -> 
                    Printf.printf "Unknown line:\n'%s'\n%!" line;
                    failwith "from_file"


let from_file path = 

    let hashtbl = Hashtbl.create 100 in

    let infile = open_in path in
    
    let rec loop n graph =
        try
            let line = input_line in

            let line = String.trim line in

            let (n2, graph2) =
    
                if line = "" then (n, graph)

                else match line.[0] with
                    | '%'   -> (n, graph)
                    | _     -> read_line n graph hashtbl line
            in

            loop n2 graph2
        with End_of_file    -> graph
    in

    let final_graph = loop 0 base_graph in
    
    close_in infile;
    final_graph;
