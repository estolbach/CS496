(* Author: Esther Stolbach
 * Date: 2/8/18
 * I pledge my honor that I have abided by the stevens honor sytsem *)


type dtree = 
	Leaf of int | Node of char * dtree * dtree


let tLeft = Node('w',Node('x', Leaf 2, Leaf 5), Leaf 8) 

let tRight = Node('w', Node('x', Leaf 2, Leaf 5), Node('y', Leaf 7, Leaf 5));;

(* 'a -> 'a -> 'a = <fun> *)
let max x y = 
	if x >= y 
	then x 
	else y 

(* dtree -> int = <fun> *)
let rec dTree_height x = 
	match x with
  	  Leaf(i) -> 0
	| Node(p,l,r)-> 1 + max (dTree_height l) (dTree_height r)

(* dtree -> int = <fun> *)
let rec dTree_size x = 
	match x with 
	  Leaf(i) -> 1
	| Node(p,l,r) -> 1 + (dTree_size l) + (dTree_size r)

(* dtree -> int list list *)
let rec dTree_paths x = 
	match x with 
	  Leaf(i) -> [[]]
	| Node(p,l,r) -> List.map (fun x -> 0::x) (dTree_paths l) 
					@List.map (fun x -> 1::x) (dTree_paths r) 

(* dtree -> bool *)
let rec dTree_is_perfect x = 
	match x with 
	  Leaf(i) -> true
	| Node(p,l,r) -> dTree_is_perfect l && dTree_is_perfect r 
	 				  && dTree_height l = dTree_height r 

(* (char -> char) -> (int -> int) -> dtree -> dtree *)
let rec dTree_map f g t = 
	match t with 
	  Leaf(x) -> Leaf(g x)
	| Node(p,l,r) -> Node(f p, dTree_map f g l, dTree_map f g r)


(* char list -> dtree *)
let rec list_to_tree list = 
	match list with
	  [] -> Leaf 0
	| h::t -> Node(h, list_to_tree t, list_to_tree t)

(* int list * int -> dtree -> dtree *)
let rec replace_leaf_helper lst dt =
	match lst, dt with
	  ([], x), Leaf(i) -> Leaf(x)
	| (h::t, x), Node(p, l, r) ->
		if (h = 0)
		then Node(p, replace_leaf_helper (t, x) l, r)
		else Node(p, l, replace_leaf_helper (t, x) r)
	| _ -> failwith "Error"

(* dtree -> (int list * int) list -> dtree *)
let rec replace_leaf_at dt g =
	match g with
	  [] -> dt
	| h::t -> replace_leaf_at (replace_leaf_helper h dt) t

(* char list * (int list * int) list -> dtree *)
let bf_to_dTree (t1, t2) =
	(replace_leaf_at (list_to_tree t1) t2)
	








