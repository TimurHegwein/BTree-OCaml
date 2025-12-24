module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module IntKV = struct
  type t = { key : int; value : int }

  (* Vergleichsfunktion der Keys *)
  let compare a b = Int.compare a.key b.key
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
    
let rec insert_aux node x k =
    let two_k = 2 * k in
    match node with
    | Leaf vals ->
        let new_vals = insert_list vals x in
        if List.length new_vals > two_k then
            (* Split *)
            let (l_vals, median, r_vals) = split_idx new_vals k in
            Split (Leaf l_vals, median, Leaf r_vals)
        else
            (* Kein Split nötig *)
            Stay (Leaf new_vals)

    | Node (vals, children) -> 
        (* "Suche" des Kind index, Fehler falls es das Element gibt*)
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

        let (left_childs, target, right_childs) = split_idx children idx in

        let ret_node = insert_aux target x k in

        match ret_node with
        | Stay new_node -> Stay (Node (vals, left_childs @ [new_node] @ right_childs))
        (* Umgang mit Split *)
        | Split (l, m, r) -> 
            (
                let new_vals = insert_list vals m in
                let new_children = left_childs @ [l; r] @ right_childs in

                let lenght_vals = List.length new_vals in

                if lenght_vals > two_k then
                    let (l_vals, m_up, r_vals)= split_idx new_vals k in
                    let (l_ch, m, r_ch) = split_idx new_children (k+1) in
                    let r_ch = m::r_ch in
                    
                    Split (Node(l_vals, l_ch), m_up, Node(r_vals, r_ch))

                else 
                    Stay (Node (new_vals, new_children))
            )
           
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