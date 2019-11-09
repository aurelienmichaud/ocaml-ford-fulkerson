open Tools

type path = string

let write_file path flownet = 
    let string_flownet = gmap flownet (fun (f, c) -> (string_of_int f) ^ "/" ^ (string_of_int c)) in
    Gfile.write_file path string_flownet

let edge_of_string str = 
    match String.split_on_char '/' str with
    | [f;c] -> (int_of_string f, int_of_string c)
    | _     -> (0,0) (* Not supposed to get there *)
    

let from_file path =
    let string_flownet = Gfile.from_file path in
    gmap string_flownet edge_of_string

let export path flownet = 
    let string_flownet = gmap flownet (fun (f, c) -> (string_of_int f) ^ "/" ^ (string_of_int c)) in
    Gfile.export path string_flownet

