(* Author: Esther Stolbach
 * Date: 1/27/18	
 * I pledge my honor that I have abided by the stevens honor sytsem *)


(* Exercise 1 *)

(* seven: 'a -> int *)
let seven t = 7

(* sign: int -> int *)
let sign x =
	if x > 0
	then 1
	else if x < 0 then -1 else 0

(* absolute: int -> int *)
let absolute x =
	if x >= 0 then
	x else x*(-1)

(* andp: (bool -> bool) -> bool *)
let andp  x y =
	match (x,y) with
	| (true, true) -> true
	| _ -> false

(* orp: (bool -> bool) -> bool *)
let orp x y =
	match (x,y) with
	| (false, false) -> false
	| _ -> true

(* notp: bool -> bool *)
let notp x =
	if x then
	 false
	else true

(* xorp: (bool -> bool) -> bool *)
let xorp x y =
	match (x,y) with
	| (true, false) -> true
	| (false, true) -> true
	| _ -> false

(* dividesBy; (int -> int) -> bool *)
let dividesBy x y =
	if x mod y = 0 then true
	else false

(* is_singleton: 'a list -> bool *)
let is_singleton x =
	match x with
	| [] -> false
	| x::y::xs -> false
	| _ -> true

(* swap: 'a * 'b -> 'b * 'a *)
let swap (x, y) = (y, x)

(* app: ('a->'b) -> 'a -> 'b *)
let app x y = x y

(* twice: ('a -> 'a) -> 'a -> 'a *)
let twice x y = x (x y)

(* comopose: ('a ->'b) -> ('c -> 'a) -> 'c -> 'b *)
let compose x y z =  x (y z)

(* Exercise 2 *)

(* belongsTo_ext: ('a -> 'a list) -> bool *)
let rec belongsTo_ext item list =
	match list with
	|[]-> false
	|x::xs -> if item = x then true
	else belongsTo_ext item xs

(* belongsTo_char: (('a -> bool) -> 'a) -> bool *)
let belongsTo_char x item =
	match (x item) with
	|true -> true
	|false -> false

(* remDups: 'a list -> 'a list 
 * helper function *)
let rec remDups = function
	|[] -> []
	|x::xs when belongsTo_ext x xs -> remDups xs
	|x::xs -> x::remDups xs

(* union_ext: ('a list -> 'a list) -> 'a list *)
let union_ext x y = remDups (x@y)

(* union_char: (('a -> bool) -> ('a -> bool) -> 'a) -> bool *)
let union_char x y z =
	if (x z) then true
	else (y z)

(* intersection_ext: ('a list -> 'a list) -> 'a list *)
let rec intersection_ext x y  =
	match y with
	 []-> []
	|h::t when belongsTo_ext h x -> remDups (h::(intersection_ext x t))
	|h::t -> remDups (intersection_ext x t)

(* intersection_char: (('a -> bool) -> ('a -> bool) -> 'a) -> bool *)
let intersection_char x y z =
	if (x z) then
	(y z) else false

(* remAdjDups: 'a list -> 'a list *)
let rec remAdjDups = function
	|[] -> []
	|[x] -> [x]
	|x::y::xs when x = y -> remAdjDups (y::xs)
	|x::y::xs -> x::remAdjDups (y::xs)

(* sublists: 'a list -> 'a list list *)
let rec sublists = function
	| [] -> [[]]
	| h::t -> let list = sublists t in
	List.map(fun f -> h::f) list@list;;

(* Exercise 3 *)

(* type *)
type calcExp =
	| Const of int
	| Add of (calcExp*calcExp)
	| Sub of (calcExp*calcExp)
	| Mult of (calcExp*calcExp)
	| Div of (calcExp*calcExp)

(* mapC : ((int -> int) -> calcExp) -> calcExp *)
let rec mapC f x =
	match x with
		| Const(a) -> Const(f a)
		| Add(a,b) -> Add((mapC f a), (mapC f b))
		| Sub(a,b) -> Sub((mapC f a), (mapC f b))
		| Mult(a,b) -> Mult((mapC f a), (mapC f b))
		| Div(a,b) -> Div((mapC f a), (mapC f b))

(* foldC: ('a -> (calcExp -> 'a -> 'a -> 'a) -> calcExp) -> 'a *)
let rec foldC x y z = 
	match z with
		| Const(a) -> y z x x 
		| Add(a, b) -> y z (foldC x y a) (foldC x y b)
		| Sub(a, b) -> y z (foldC x y a) (foldC x y b)
		| Mult(a, b) -> y z (foldC x y a) (foldC x y b)
		| Div(a, b) -> y z (foldC x y a) (foldC x y b)

(* numAdd : calcExp -> int *)
let numAdd exp =
	let count exp x y = 
	match exp with
		| Add(a, b) -> 1 + x + y
		| _ -> x + y in
	foldC 0 count exp

(* replaceAddWithMult : calcExp -> calcExp *)
let rec replaceAddWithMult exp =
	let r exp x y = 
	match exp with
		| Const(a) -> Const(a)
		| Add(a, b) -> Mult(x, y)
		| Sub(a, b) -> Sub(a, b)
		| Mult(a, b) -> Mult(a, b)
		| Div(a, b) -> Div(a, b) in
	foldC (Const(0)) r exp

(* evalC : calcExp -> int *)
let rec evalC exp = 
	match exp with
		| Const(a) -> a
		| Add(a, b) -> evalC a + evalC b
		| Sub(a, b) -> evalC a - evalC b
		| Mult(a, b) -> evalC a * evalC b
		| Div(a, b) -> evalC a / evalC b

(* evalCf : calcExp -> int *)
let evalCf exp =
	let f exp x y = 
	match exp with
		| Const(a) -> a
		| Add(a, b) -> x + y
		| Sub(a, b) -> x - y
		| Mult(a, b) -> x * y
		| Div(a, b) -> x / y in
	foldC 0 f exp

(* Exercise 4*)

(* This functions counts the number of even numbers given in an int list
 * and returns the amount of times an even number appears
 * ex. given a list: [1;2;3;4;5;6;7;8;9;10] it will return 5
 * because there are 5 even numbers in the list, 2,4,6,8,10.  
 * f: int list -> int *)
let f xs =
let g = fun x r -> if x mod 2 = 0 then (+) r 1 else r
in List.fold_right g xs 0

(* append: ('a list -> 'a list ) -> 'a list *)
let append xs =
	let g = fun x h -> x::h
	in List.fold_right g xs