open OUnit2
open Ast
open Ds
open Interp

(* A few test cases *)
let tests = [
  "int"  >:: (fun _ -> assert_equal (NumVal 22) (interp "22"));
  "add"  >:: (fun _ -> assert_equal (NumVal 22) (interp "11+11"));
  "adds" >:: (fun _ -> assert_equal (NumVal 22) (interp "(10+1)+(5+6)"));
  "let"  >:: (fun _ -> assert_equal (NumVal 22) (interp "let x=22 in x"));
  "lets" >:: (fun _ -> assert_equal (NumVal 22) (interp "let x = 0 in let x = 22 in x"));
  "multiple_args" >:: (fun _ -> assert_equal (NumVal 11) (interp "(((proc(x,y) {x+y}) 5) 6)"));
  "for_loop" >:: (fun _ -> assert_equal (NumVal 10)
                                        (interp ("let x = newref(0) in
                                                 begin
                                                    for i=1 to 10 (
                                                        setref(x, deref(x) - (-1))"
                                                        (*BREAKPOINT*)^
                                                    ");
                                                    deref(x)
                                                 end")));

]

let _ = run_test_tt_main ("suite" >::: tests)

(*
ENV
----------
id | val
--------------
x  | RefVal 0
i  | NumVal 1
*)


(*
STORE
------
mem | val
---------------
 0  | NumVal 1
*)
