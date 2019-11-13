open Printf
open Graph
open Tools

type path = string

let get_id hashtbl n name =
    let name = String.trim name in
    try
        (n, Hashtbl.find hashtbl name)
    with Not_found  -> 
        Hashtbl.add hashtbl name (n+1); (n+1, n+1)
        

let read_capacity_line n graph hashtbl name capacity = 
  
    (* If the name is already in the Hashtbl, we update it 
     * If not, we add it to the hashtbl *)
    let (n, name_id) = get_id hashtbl n name in

    let (n, sink_id) = get_id hashtbl n "__sink__" in

    (* Add name to the graph if it doesn't exist *)
    let graph =
        try
            new_node graph name_id
        with (Graph_error (e)) -> graph
    in

    (n, new_arc graph name_id sink_id (0, int_of_string capacity))

(* weight is not used for now *)
let read_connection_line n graph hashtbl weight froms tos = 

    let weight = int_of_string weight in

    let froms = String.split_on_char ',' froms in
    let tos = String.split_on_char ',' tos in

    let (n, source_id) = get_id hashtbl n "__source__" in
    let (n, sink_id) = get_id hashtbl n "__sink__" in

    (* Buggy but i need to go to sport class, so i'll see tonight *)
    let rec link_to graph n from_id = function
        | []    -> (n, graph)
        | h::t  -> 
            let (n, h_id) = get_id hashtbl n h in
            (* Add h to the graph if it doesn't exist *)
            let graph =
                try
                    new_node graph h_id
                with (Graph_error (e)) -> graph
            in
            (* Add arc from_id -> h_id *)
            link_to
                (new_arc graph from_id h_id (0, 1))
                n
                from_id
                t
    in


    let rec link_all graph n = function
        | []    -> (n, graph)
        | h::t  -> 
            (* Fetch 'h' 's id in the hash table *)
            let (n, from_id) = get_id hashtbl n h in
            (* Add h to the graph if it doesn't exist *)
            let graph =
                try
                    new_node graph from_id
                with (Graph_error (e)) -> graph
            in
            (* Add arc from source node to 'h' node *)
            let graph = new_arc graph source_id from_id (0, 1) in

            let (n, new_graph) = link_to graph n from_id tos in
            link_all new_graph n t
    in

    
    let (n, graph) = link_all graph n froms in

    (* Link all the right column of the bipartite problem with the sink node *)
    let (n, graph) =
        List.fold_left
        (fun (n,gr) t -> 
            let (n, t_id) = get_id hashtbl n t in
            try
                match find_arc gr t_id sink_id with
                    | None  -> (n, new_arc gr t_id sink_id (0, 1))
                    | _ -> (n, gr)

            with (Graph_error (e)) -> (n, new_arc gr t_id sink_id (0, 1)))
        (n, graph)
        tos
    in
    (n, graph)
     


(* NameX[,NameY,...]=NameJ[,NameK,...]*)
let get_connection_line line =
    match String.split_on_char '=' line with
        | [froms;tos]   -> Some (String.trim froms, String.trim tos)
        | _             -> None

(* Weight|NameX[,NameY,...]=NameJ[,NameK,...]*)
let get_weighed_connection_line line =
    match String.split_on_char '|' line with
        | [weight;rest] -> 
            begin
            match get_connection_line rest with
                | Some (froms, tos) -> Some (String.trim weight, String.trim froms, String.trim tos)
                | _ -> None
            end
        | _ -> None


(* Name:Capacity, e.g. :
 * University1:20 *)
let get_capacity_line line = 
    match String.split_on_char ':' line with
        | [name;capacity]   -> Some (String.trim name, String.trim capacity)
        | _                 -> None

let read_line n graph hashtbl line =

    match get_capacity_line line with
        | Some (name, capacity) -> read_capacity_line n graph hashtbl name capacity
        | _ -> 

        match get_weighed_connection_line line with
            | Some (weight, froms, tos) -> read_connection_line n graph hashtbl weight froms tos
            | _ ->
    
            match get_connection_line line with
                | Some (froms, tos) -> read_connection_line n graph hashtbl "0" froms tos
                | _ -> 
                    Printf.printf "Unknown line:\n'%s'\n%!" line;
                    failwith "from_file"


let from_file path = 

    let hashtbl = Hashtbl.create 100 in

    (* Add source and sink nodes in the flow_network *)
    let base_graph = new_node empty_graph 9998 in
    let base_graph = new_node base_graph 9999 in
    (* Add source and sink nodes in the hash table *)
    Hashtbl.add hashtbl "__source__" 9998;
    Hashtbl.add hashtbl "__sink__" 9999;

    let infile = open_in path in
    
    let rec loop n graph =
        try
            let line = input_line infile in

            let line = String.trim line in

            let (n2, graph2) =
    
                if line = "" then (n, graph)

                else match line.[0] with
                    (* Comment *)
                    | '%'   -> (n, graph) (* Line to be parsed *)
                    | _     -> read_line n graph hashtbl line
            in

            loop n2 graph2
        with End_of_file -> graph
    in
    

    let final_graph = loop 0 base_graph in
    
    close_in infile;

    let hashtbl_file = open_out (path ^ "._translate") in

    Hashtbl.iter
    (fun name id -> 
        fprintf hashtbl_file "%s:%d\n" name id)
    hashtbl;

    close_out hashtbl_file;

    final_graph

    
let export bg input_path output_path = 

    let infile = open_in (input_path ^ "._translate") in

    let hashtbl = Hashtbl.create 100 in

    let rec loop () =
        try
            let line = input_line infile in

            let line = String.trim line in
        
            match String.split_on_char ':' line with
                | [name;id] -> Hashtbl.add hashtbl id name; loop ()
                | _ -> Printf.printf "Translation impossible"; failwith "export"
    
        with End_of_file -> ()
    in
    loop ();

    let output_file = open_out (output_path ^ ".dot") in 

    fprintf output_file "digraph finite_state_machine {\n";
    fprintf output_file "\trankdir=LR;\n";
    fprintf output_file "\tnode [shape = circle];\n";

    e_iter bg (fun id1 id2 (flow,capacity) -> 
                if flow <> 0 && id1 <> 9998 && id2 <> 9999 then                
                    let name1 = Hashtbl.find hashtbl (string_of_int id1) in
                    let name2 = Hashtbl.find hashtbl (string_of_int id2) in
                    fprintf output_file "\t%s -> %s [ label = \"\" ];\n" name1 name2 
                else ());


    fprintf output_file "}\n";

    close_out output_file


