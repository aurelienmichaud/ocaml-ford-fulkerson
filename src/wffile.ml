open Weighedflownetwork
open Printf
open Graph

type path = string

let export path wfn =
    let outfile = open_out (path ^ ".dot") in

    fprintf outfile "digraph finite_state_machine {\n";
    fprintf outfile "\trankdir=LR;\n";
    fprintf outfile "\tnode [shape = circle];\n";

    e_iter wfn (fun id1 id2 (f, c, w) ->
                    fprintf outfile "\t%d -> %d [ label = \"%s\" ];\n"
                        id1
                        id2
                        ((string_of_int f) ^
                        "/" ^
                        (string_of_int c) ^
                        " (" ^
                        (string_of_int w) ^
                        ")"));

    fprintf outfile "}\n";
    close_out outfile;
    ()


