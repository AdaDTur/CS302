(* Question 1: Tree Depth *)
(* TODO: Write a good set of tests for tree depth. *)
let tree_depth_cps_tests : (int tree * int) list =
  [
    ((Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty))),(2));
    (Empty,(0));
    ((Tree (Empty, 1, Empty)), (1));
    ((Tree (Tree (Empty, 2, Empty), 1, Tree (Tree (Empty, 2 ,Empty), 3, Empty))), (3));
  ]

(* An example of Non-CPS function to find depth of a tree: *)
let rec tree_depth t =
  match t with
  | Empty -> 0
  | Tree (l, _, r) -> 1 + max (tree_depth l) (tree_depth r)

(* TODO: Implement a CPS style tree_depth_cps function.*)
let tree_depth_cps (t: 'a tree) = 
  let rec helper (t: 'a tree) (sc: (int -> int)) =
    match t with
    | Empty -> sc 0
    | Tree (l, _, r) -> helper l (fun l -> helper r (fun r -> sc (max l r + 1)))
  in helper t (fun x -> x)

(* Question 2(a): Tree Traversal *)
(* TODO: Write a good set of tests for testing your tree traversal function. *)
let traverse_tests : (int tree * int list) list = [
  (Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)),([2; 3; 1]));
]

(* TODO: Implement a CPS style postorder traversal function. *)
let traverse (tree : 'a tree) : 'a list = 
  let rec helper (tree : 'a tree) (sc: 'a list -> 'a list) = 
    match tree with
    | Empty -> sc []
    | Tree (l, node, r) -> helper l (fun l -> helper r (fun r -> sc (l @ r @ [node])))
  in
  helper tree (fun x -> x)

(* Question 2(b): Distances from the Root *)
(* TODO: Write a good set of tests for testing the get_distances function. *)
let get_distances_tests : (int tree * int list) list = [
  ((Tree (Tree (Empty, 3, Empty), 5, Tree (Empty, 6, Empty))),([8; 11; 5]));
]

(* TODO: Implement a CPS style get_distances function. *)
let get_distances (tree : int tree) : int list = 
  let rec helper tree sum sc =
    match tree with
    | Empty -> sc []
    | Tree (l, node, r) -> helper l (sum + node) (fun l -> helper r (sum + node) (fun r -> sc (l @ r @ [sum + node])))
  in
  helper tree 0 (fun x -> x)

(* Question 3: Finding Subtrees *)
(* TODO: Write a good set of tests for finding subtrees. *)
let find_subtree_cps_tests : ((int list * int tree) * int tree option) list =
  [ 
    (([2;1],(Tree (Tree (Empty, 2, Empty), 1, Empty))),(None));
    (([0;1],Tree (Empty, 0, Tree(Empty, 1, Empty))), (Some (Empty)));
  ]

(* TODO: Implement a CPS style find_subtree_cont function.*)
let find_subtree_cps ls tree =
  let rec helper ls tree sc fc = 
    match ls with
    | [] -> sc tree
    | x::xs ->
        match tree with
        | Empty -> fc
        | Tree (l, node, r) ->
            if x = node then
              helper xs l sc fc
            else
              helper ls r sc (helper ls l sc fc)
  in
  helper ls tree (fun x -> Some x) None



