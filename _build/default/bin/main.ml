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
    
let update_idx l new_val idx =
    let (_, acc) = List.fold_left (fun (i, acc) a -> if i = idx then (i+1, new_val::acc) else (i+1, a::acc)) (0, []) l
    in
    List.rev acc  

let split_idx l idx = 
    let rec aux l i acc 


(* Split Funktion
    returns (left_node, [median], right_node)
*)
let rec split node two_k= 
    let median_idx = (two_k + 1) / 2 in
    let median_idx2 = (two_k + 2) / 2 in
    let rec split_vals vals i acc = 
        let (left, median ,right) = acc in
        match vals with
        | [] -> acc
        | x::xs -> (
            if i < median_idx then
                split_vals xs (i + 1) (x::left, median,right)
            else if i > median_idx then
                split_vals xs (i + 1) (left, median,x::right)
            else split_vals xs (i + 1) (left, x::median ,right)
        )
    in
    let rec split_childs childs i acc = 
        let (left, right) = acc in
        match childs with
        | [] -> acc
        | x::xs -> (
            if i < median_idx2 then
                split_childs xs (i + 1) (x::left,right)
            else if i > median_idx then
                split_childs xs (i + 1) (left,x::right)
            else split_childs  xs (i + 1) (left ,right)
        )
    in
    match node with
    | Leaf (vals) -> (
        let (left, median, right) = split_vals vals 0 ([], [], []) in
        (Leaf (List.rev left), median, Leaf (List.rev right))
    )
    | Node (vals, childs) -> (
        let (left, median, right) = split_vals vals 0 ([], [], []) in
        let (left_ch, right_ch) = split_childs childs 0 ([], []) in
        (Node (List.rev left, List.rev left_ch), median, Node (List.rev right, List.rev right_ch))
    )
    
(* uses median and the nodes to create a new (updated) upper node*)
let handle_split (left, median, right) vals_up children_up =
    let m = 
        match median with
        | [] -> failwith "This is should never occur: We should split but there is no median"
        | m::_ -> m
    in
    let new_vals = insert_list vals_up m 
    in 
    (*tmp ist der Index vom eingefügten median in vals*) 
    let tmp = (List.find_index (fun a -> a = m) new_vals)
    in
    let idx = 
        match tmp with 
        | None -> failwith "this should never happen, the median was not inserted in the new vals"
        | Some x -> x
    in
    let (new_ch, _) = 
        List.fold_left (
        fun (ch, i) a -> (
            if i = idx then
                (right::left::ch, i+1)
            else
                (a::ch, i+1)
            )
        ) ([], 0) children_up
    in
    let new_ch = List.rev (new_ch)
    in
    Node (new_vals, new_ch)

let rec insert_aux node value k =
    match node with
    | Leaf (vals) ->
        Leaf (insert_list vals value)
    | Node (vals_up, children_up) -> 
        (* Insert the value*)
        let rec helper vals childs idx= 
            match vals, childs with
            | [], [] -> failwith "Invalid State should not be reached"
            | _::_, [] -> failwith "Invalid State should not be reached"
            | [], x::xs -> if xs = [] then (insert_aux x value k), idx else failwith "Too many children should not be reached"
            | cur_val::rest_val, cur_ch::rest_ch -> (
                let cmp = Ord.compare cur_val value in
                if cmp = 0 then 
                    failwith "Value is already inserted"
                else if cmp > 0 then
                    (insert_aux cur_ch value k), idx
                else (* cur_val < value *)
                    helper rest_val rest_ch (idx + 1)
            )
        in
        let ret_node, child_idx = helper vals_up children_up 0 in
        let two_k = 2 * k 
        in
        match ret_node with
        | Leaf (vals) -> (
            if List.length vals > two_k then 
                handle_split (split (Leaf (vals)) two_k) vals_up children_up
            else Node (vals_up, (update_idx children_up ret_node child_idx)))
        | Node (vals, children) -> (
            if List.length vals > two_k then
                handle_split (split (Node (vals, children)) two_k) vals_up children_up
            else Node (vals_up, (update_idx children_up ret_node child_idx))
        )
let insert tree x =
    let Tree (k, root) = tree in
    let new_root = (insert_aux root x k) in
    (* Muss die Wurzel geteilt werden ?*)
    let new_root = 
        match new_root with
    | Leaf (vals) -> (
        if List.length vals > 2 * k then
            let (left, median, right) = split (Leaf (vals)) (2*k) in
            Node (median, [left; right])
        else
            Leaf (vals)
        )
    | Node (vals, children) -> (
        if List.length vals > 2 * k then
            let (left, median, right) = split (Node (vals, children)) (2*k) in
            Node (median, [left; right])
        else
            Node (vals, children)
        )
    in
    Tree (k, new_root)

end


(* --- Test-Bereich --- *)

(* 1. Den Baum für Integers erstellen *)
module IntBTree = MakeBTree(Int)

(* 2. Test-Ablauf definieren *)
let run_test () =
  let k = 2 in
  let tree = IntBTree.init_tree k in
  
  Printf.printf "Starte Test mit k=%d...\n" k;
  
  (* Mehrere Werte einfügen *)
  let values = [10; 20; 5; 15; 30; 25; 40] in
  let final_tree = List.fold_left (fun acc v -> 
    Printf.printf "Füge %d ein...\n" v;
    IntBTree.insert acc v
  ) tree values in

  (* Prüfen, ob alle Werte gefunden werden *)
  Printf.printf "\nPrüfe Werte:\n";
  List.iter (fun v -> 
    let found = IntBTree.lookup final_tree v in
    Printf.printf "Wert %d gefunden? %b\n" v found
  ) (values @ [99; 0]); (* 99 und 0 sollten false sein *)
  
  Printf.printf "\nTest abgeschlossen.\n"

(* 3. Den Test tatsächlich ausführen *)
let () = run_test ()