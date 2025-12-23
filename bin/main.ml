type node = 
    | Empty_Node
    | Leaf of int list
    | Node of int list * node list

type btree = Tree of int * node


let test1 = Tree(1, 
    Node([3; 7], 
            [Node([1; 2], [Empty_Node; Empty_Node; Empty_Node])
            ; Node([4; 5], [Empty_Node; Empty_Node; Empty_Node])
            ; Node([8; 9], [Empty_Node; Empty_Node; Empty_Node])]
        )
    )

let test_deeper = Tree(1, 
    Node([5], 
            [Node([2], 
                [Node([1], [Empty_Node; Empty_Node])
                ; Node([3], [Empty_Node; Empty_Node])]
            )
            ; Node([9], 
                [Node([8], [Empty_Node; Empty_Node])
                ; Node([10], [Empty_Node; Empty_Node])]
            )]
        )
    )

let lookup tree x =
    let Tree (_, root) = tree in
        (* Search Helper Function*)
    let rec search node = 
        match node with
        | Leaf (vals) -> List.exists (fun t -> t = x) vals
        | Empty_Node -> false
        | Node (vals, children) -> 
            let rec iter_list vals children =
                match vals, children with
                | [], [] -> false
                | _::_, [] -> failwith ("Invalid State: More vals then children")
                | [], last_ch::invalid_rest -> if invalid_rest = [] then search last_ch else failwith "More then one Child left even though no vals"
                | cur_val::rest_val, cur_ch::rest_ch -> (
                    if x = cur_val then
                        true
                    else if x > cur_val then
                        iter_list rest_val rest_ch
                    else if x < cur_val then
                        search cur_ch
                    else 
                        failwith "Invalid state"
                )
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
            if x > value then 
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

let test_l = [1; 2; 4]

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
    | Empty_Node -> failwith "Invalid"
    | Leaf (vals) -> (
        let (left, median, right) = split_vals vals 0 ([], [], []) in
        (Leaf (left), median, Leaf (right))
    )
    | Node (vals, childs) -> (
        let (left, median, right) = split_vals vals 0 ([], [], []) in
        let (left_ch, right_ch) = split_childs childs 0 ([], []) in
        (Node (left, left_ch), median, Node (right, right_ch))
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
    | Empty_Node -> node (*should never be reached*)
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
                if cur_val = value then 
                    failwith "Value is already inserted"
                else if cur_val > value then
                    (insert_aux cur_ch value k), idx
                else (* cur_val < value *)
                    helper rest_val rest_ch (idx + 1)
            )
        in
        let ret_node, child_idx = helper vals_up children_up 0 in
        let two_k = 2 * k 
        in
        match ret_node with
        | Empty_Node -> ret_node
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
    (* Init root node*)
    if root = Empty_Node then
        let new_root = Leaf ([x]) in
        Tree (k, new_root)
    else
        let new_root = (insert_aux root x k) in
        (* Muss die Wurzel geteilt werden ?*)
        let new_root = 
            match new_root with
        | Empty_Node -> new_root
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