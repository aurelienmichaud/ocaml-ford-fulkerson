open Tools
open Bipartite
open Flownetwork
open Weighedflownetwork
(*open Bfile*)

let handle_default infile outfile =
    Printf.printf "Default behaviour : export graph into .dot format\n%!";
    let graph = Gfile.from_file infile in
    let () = Gfile.export outfile graph in
    Printf.printf "Done.\n%!";
    ()

let handle_bipartite infile outfile =
    Printf.printf "Bipartite Graph solver\n%!";
    (* Open file *)
    let graph = Bfile.from_file infile in

    let new_graph = Bipartite.solve graph in

    (* Export the infile graph as a SVG file to get a reference to look at *)
    (*let () = Ffile.export infile graph  in*)
    (* Export the new graph in SVG format in the same shape as the reference graph *)
    let () = Bfile.export new_graph infile outfile in
    Printf.printf "Done.\n%!";
    ()

let handle_ford_fulkerson infile outfile source sink =
    Printf.printf "Flow Network solver\n%!";
    let graph = Ffile.from_file infile in

    let new_graph = Flownetwork.ford_fulkerson graph source sink in

    (* Export the infile graph as a SVG file to get a reference to look at *)
    let () = Ffile.export infile graph  in
    (* Export the new graph in SVG format in the same shape as the reference graph *)
    let () = Ffile.export_same_shape outfile graph new_graph in
    Printf.printf "Done.\n%!";
    ()

let () =

(* Check the number of command-line arguments *)
    if Array.length Sys.argv <> 5 then
    begin
        Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
        exit 0
    end ;


    (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
    let infile = Sys.argv.(1)
    and outfile = Sys.argv.(4)

    (* These command-line arguments are not used for the moment. *)
    and _source = int_of_string Sys.argv.(2)
    and _sink = int_of_string Sys.argv.(3)
    in

    (* Check the input file extension *)
    match (List.rev (String.split_on_char '.' infile)) with
        | "bprtt"::t    -> handle_bipartite infile outfile;
        | "flwnt"::t    -> handle_ford_fulkerson infile outfile _source _sink;
        | _             -> handle_default infile outfile;


    ()

