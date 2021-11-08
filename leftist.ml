(******************************************)
(*         ZADANIE DRZEWA LEWICOWE        *)
(*        ROZWIAZANIE: MARCIN ZOLEK       *)
(*          RIWJU: MAJA TKACZYK           *)
(******************************************)

exception Empty;;
type 'a queue = Leaf | Node of 'a queue * 'a queue * 'a * int;;
(* typ 'a queue przechowuje informacje odpowiednio o lewym poddrzewie, 
prawym poddrzewie, wartosci priorytetu, dlugosci skrajnie prawej sciezki *)

let merge tree1 tree2 =
    (* laczenie drzew *)

    let swap (tree1, tree2) =
        (* zamienia kolejnosc drzew, tak aby pierwsze mialo element priorytetowy *)
        match (tree1, tree2) with
        | (Leaf, Leaf) -> (tree1, tree2)
        | (Leaf, _) -> (tree1, tree2)
        | (_, Leaf) -> (tree2, tree1)
        | (Node (_, _, priority1, _), Node (_, _, priority2, _)) ->
            if priority1 < priority2 then
                (tree1, tree2)
            else
                (tree2, tree1)
    in
    
    let rec main (tree1, tree2) =
        (* glowna funkcja zlaczajaca drzewa *)
    
        match (tree1, tree2) with
        | (Leaf, Leaf) -> Leaf
        | (Leaf, _) -> tree2
        | (_, Leaf) -> tree1
        | (Node (leftChild1, rightChild1, priority1, height1), Node (leftChild2, rightChild2, priority2, height2)) ->
            let tree3 = main (swap (rightChild1, tree2)) in 
            (* drzewo powstale w wyniku scalenia prawego poddrzewa drzewa 1 z drzewem 2 *)
            
            match (leftChild1, tree3) with
            | (Leaf, Node (_, _, _, height3)) -> (Node (tree3, leftChild1, priority1, 1))
            | (Node (_, _, _, height4), Node (_, _, _, height3)) -> 
                if height3 < height4 then 
                    (Node (leftChild1, tree3, priority1, height3 + 1))
                else
                    (Node (tree3, leftChild1, priority1, height4 + 1))
    in
    
    main (swap (tree1, tree2))
;;

let empty = 
    Leaf
;;

let is_empty q =
    match q with 
    | Leaf -> true
    | _ -> false
;;

let add e q =
    merge (Node (Leaf, Leaf, e, 1)) q
;;

let delete_min q =
    if is_empty q then
        raise Empty
    else
        let (Node (leftChild, rightChild, priority, height)) = q in
        
        (priority, merge leftChild rightChild)
;;

let join q1 q2 = 
    merge q1 q2
;;
