open Graph

let clone_nodes gr = n_fold gr (fun g id -> new_node g id) empty_graph

let gmap gr f =
    let clone_gr = clone_nodes gr in
    e_fold gr (fun g id1 id2 lbl -> new_arc g id1 id2 (f lbl)) clone_gr
    
let add_arc g id1 id2 n = 
        match find_arc g id1 id2 with
        | None          -> new_arc g id1 id2 n
        | Some a        -> new_arc g id1 id2 (a + n)
                             
       


