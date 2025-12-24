module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module MakeBTree (Ord : OrderedType) = struct
type element = Ord.t

type node = 
    | Leaf of element list
    | Node of element list * node list

type btree = Tree of int * node

(* Wird genutzt um im Backtracking mit Updates umzugehen *)
type insert_res = 
  | Stay of node               
  | Split of node * element * node

let init_tree k = Tree (k, Leaf([]))

(* Hilfsunktionen *)
(* insert_list: Sortiert einfügen von value in Liste l *)
let insert_list l value = 
    let rec aux l value acc =
        match l with
        | [] -> List.rev (value::acc)
        | x::xs -> (
            let cmp = Ord.compare x value in
            if cmp > 0 then 
                List.concat [List.rev acc; value :: [x]; xs]
            else
                aux xs value (x::acc)
        )
    in
    aux l value []

(* Split der Liste l an Index idx, Rückgabe: (links: list, median: element, rechts:list) *)
let split_idx l idx = 
    let rec aux l i acc =
        match l with
        | [] -> List.rev acc, None, []
        | x :: xs -> (
            if i = idx then (List.rev acc, Some x, xs)
            else aux xs (i+1) (x::acc) 
        )
    in
    let (a, med, b) =  aux l 0 [] 
    in
    match med with
    | Some m ->(a, m, b)
    | None -> failwith "Split hat nicht funktioniert"

(* Hauptfunktionen *)
(* Suche nach Wert im Baum -> true/false *)
let lookup tree x =
    let Tree (_, root) = tree in
    (* Search Helper Function*)
    let rec search node = 
        match node with
        | Leaf (vals) -> List.exists (fun t -> Ord.compare t x = 0) vals
        | Node (vals, children) -> 
            let rec iter_list vals children =
                match vals, children with
                | [], [last_ch]-> search last_ch
                | cur_val::rest_val, cur_ch::rest_ch -> (
                    let cmp = Ord.compare x cur_val in
                    if cmp = 0 then true
                    else if cmp < 0 then search cur_ch
                    (* cmp > 0 *)    
                    else iter_list rest_val rest_ch
                )
                | _ -> failwith "Invalid state: Sollte nicht vorkommen (bspw. zu viele oder wenige Kinder etc.)"
            in
            (* Aufruf der Suche*)
            iter_list vals children
    in 
    (* Helper call*)
    search root

(* lookup -> gefundenert Wert*)
let lookup_value tree x =
    let Tree (_, root) = tree in
    (* Search Helper Function*)
    let rec search node = 
        match node with
        | Leaf (vals) -> List.find_opt (fun t -> Ord.compare t x = 0) vals
        | Node (vals, children) -> 
            let rec iter_list vals children =
                match vals, children with
                | [], [last_ch]-> search last_ch
                | cur_val::rest_val, cur_ch::rest_ch -> (
                    let cmp = Ord.compare x cur_val in
                    if cmp = 0 then Some (cur_val)
                    else if cmp < 0 then search cur_ch
                    (* cmp > 0 *)    
                    else iter_list rest_val rest_ch
                )
                | _ -> failwith "Invalid state: Sollte nicht vorkommen (bspw. zu viele oder wenige Kinder etc.)"
            in
            (* Aufruf der Suche*)
            iter_list vals children
    in 
    (* Helper call*)
    search root

(* Rekursive Logik für den Insert*)
(**)
let rec insert_aux node x k =
    let two_k = 2 * k in
    match node with
    | Leaf vals ->
        let new_vals = insert_list vals x in
        (* Splitkontrolle *)
        if List.length new_vals > two_k then
            let (l_vals, median, r_vals) = split_idx new_vals k in
            Split (Leaf l_vals, median, Leaf r_vals)
        else Stay (Leaf new_vals)

    | Node (vals, children) -> 
        (* Suche des Kind Index, in den das Element muss, Fehler falls es das schon Element im Baum ist *)
        let rec find_ch_idx i vals = 
            match vals with
            | t::rt -> (
                let cmp = Ord.compare x t in
                if cmp > 0 then find_ch_idx (i + 1) rt
                else if cmp = 0 then failwith "Element schon vorhanden und darf nicht eingefügt werden"
                else i)
            | [] -> i
        in
        let idx = find_ch_idx 0 vals in

        (* in target wird value eingefügt *)
        let (left_childs, target, right_childs) = split_idx children idx in

        (* retnode ist das Ergebnis des Einfügens *)
        let ret_node = insert_aux target x k in
        match ret_node with
        (* Falls kein Split nötig war: Einfach target druch den neuen node ersetzen *)
        | Stay new_node -> Stay (Node (vals, left_childs @ [new_node] @ right_childs))
    
        (* In target war ein Split nötig, nun könnte es sein, dass dieser Knoten auch geteilt werden muss *)
        | Split (l, m, r) -> 
            (
                (* Einfügen des Medians und enstandener Children aus target *)
                let new_vals = insert_list vals m in
                let new_children = left_childs @ [l; r] @ right_childs in

                (* Kontrolle ob dieser Knoten geteilt werden muss *)
                let lenght_vals = List.length new_vals in
                if lenght_vals > two_k then
                    let (l_vals, m_up, r_vals)= split_idx new_vals k in

                    (* Aufteilen der Kinder (genaue Erklärung):
                    - Bei n values gibt es n+1 Kinder
                    - Bei 2k + 1 values also 2k+2 = 2 (k+1) Kinder
                    -> Somit split bei k+1
                    -> Median gehört zur rechten Liste
                    *)
                    let (l_ch, m, r_ch) = split_idx new_children (k+1) in
                    let r_ch = m::r_ch in
                    
                    Split (Node(l_vals, l_ch), m_up, Node(r_vals, r_ch))

                else Stay (Node (new_vals, new_children))
            )
   
(* Aufruf von Insert und ggf. Wurzel Split*)
let insert tree x =
    let Tree (k, root) = tree in
    let new_root = (insert_aux root x k) in
    (* Muss die Wurzel geteilt werden ?*)
    let new_root = 
        match new_root with
        | Stay new_r -> new_r
        | Split (l, m, r) -> Node([m], [l; r])
    in
    Tree (k, new_root)

end




module IntKV = struct
  type t = { key : int; value : int }

  (* Vergleichsfunktion *)
  let compare a b = Int.compare a.key b.key
end

module IntKVBtree = MakeBTree(IntKV)
module IntBtree = MakeBTree(Int)

(* Beispiel: Studenten und ihre Noten *)
let run_main () =
    let k = 4 in
    let tree = IntKVBtree.init_tree k in
    let tree_for_unique_mats = IntBtree.init_tree k in
    let num_students = 50 in

    let _ = Random.self_init () in

    let rec get_unique_mat tree_for_unique_mats =
        let mat = Random.int 51 in 
        if IntBtree.lookup tree_for_unique_mats mat then get_unique_mat tree_for_unique_mats
        else (mat, IntBtree.insert tree_for_unique_mats mat)
    in
    let rec fill_tree tree tree_for_unique_mats num_students =
        if num_students = 0 then tree
        else 
            let (mat, tree_for_unique_mats) = get_unique_mat tree_for_unique_mats in
            let grade = 1 + Random.int 6 in

            fill_tree (IntKVBtree.insert tree {key = mat; value = grade}) tree_for_unique_mats (num_students-1)

    in
    let tree = fill_tree tree tree_for_unique_mats num_students in
    
    let tests = 20 in
    Printf.printf "Starte lookups mit t=%d zufälligen keys \n" tests;

    let rec test tree num_tests =
        if num_tests = 0 then Printf.printf "\nTest abgeschlossen.\n"
        else
            let rndm_mat = Random.int 51 in
            let student = IntKVBtree.lookup_value tree {key=rndm_mat; value=0} in

            match student with
            | None -> Printf.printf "Gesuchter key: k=%d NICHT Gefunden\n" rndm_mat; test tree (num_tests - 1)
            | Some (s) -> Printf.printf "Gesuchter key: k=%d Gefunden! Note: s=%d\n" rndm_mat s.value; test tree (num_tests - 1)  
        in
    test tree 20

let () = run_main ()