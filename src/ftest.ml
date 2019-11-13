open Ffile
open Tools
open Bipartite
(*open Bfile*)

let testit g source sink =
    (*ford_fulkerson g source sink*)
    solve g

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

    (* Open file *)
    let graph = Bfile.from_file infile in

    (* TEST CODE HERE *)

    let new_graph = testit graph _source _sink in

    (* END OF TEST CODE HERE *) 
    (* Rewrite the graph that has been read. *)
    (*let () = write_file outfile new_graph in*)

    (* Export the infile graph as a SVG file to get a reference to look at *)
    let () = Ffile.export infile graph  in
    (* Export the new graph in SVG format in the same shape as the reference graph *)
    let () = Bfile.export new_graph infile outfile in

    ()

