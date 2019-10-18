(* 
                Mutable points with vector arithmetic
                          CS51 Problem Set 8
                         -.-. ... ..... .----
 *)

open Expr ;;
open Test_simple ;;
open Evaluation ;;

(* type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float                           (* integers *)
  | String of string                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;; *)

(************* 			Expr Testing: 			*************
	 The trickiest part of expr has been let binding. I have explicitly tested it 
	 below. The rest of expr is assumed to work if evalutions testing works.  
***********************************************************)

let extract_e (v : Env.value) : expr = 
  match v with 
  | Val e -> e 
  | Closure (e, _) -> e ;; 

let let1 = Let("x", Binop(Plus,Var("x"), Num(2)), 
					 Binop(Plus, Var("x"), Num(4))) ;; 
let subst1 = exp_to_concrete_string (subst "x" (Num(5)) let1) ;;

let env1 = Env.extend (Env.create ()) "x" (ref (Env.Val (Num(4)))) ;; 

let let2 = Let("y", Binop(Plus,Var("x"), Num(2)), 
					 Binop(Plus, Var("x"), Num(4))) ;; 
let subst2 = exp_to_concrete_string (subst "x" (Num(5)) let2) ;;

let let3 = Let("y", Binop(Plus, Var("x"), Num(2)), 
					 Binop(Plus, Var("x"), Var("y"))) ;; 
let subst3 = exp_to_concrete_string (subst "x" (Var("y")) let3) ;;

let subst_tests () = 
	unit_test (subst1 = "let x = 5 + 2 in x + 4") "Let case 1"; 
	unit_test (subst2 = "let y = 5 + 2 in 5 + 4") "Let case 2"; 
	unit_test (subst3 = "let var0 = y + 2 in y + var0") "Let case 3"; 
	() ;; 

(************* 			Evaluation Testing: 			*************
	 I'm assuming by testing my evaluations I'm also testing to see if my module
	 for Env is working. It's also implicitly checking to see if my functions in 
	 expr.ml are working, so the bulk of my testing is for the three types of 
	 evaluations I'm doing. e1 - e3 tests are for ints while e4 - e6 are for 
	 floats and e7 is for strings. These tests should be comprehensive for 
	 evaluation.ml and expr.ml. 
***********************************************************)

(* 
	let x = 1 in 
	let f = fun y -> x + y in
	let x = 2 in
	f 3 ;; 
*)
let e1 = Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))),
		 Let("x", Num(2), App(Var("f"), Num(3))))) ;;

(* 
	let rec f = fun x -> if x = 0 then ~-1 else x * f (x - 1) in f 4 ;;
*)
let e2 = Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), 
				 Unop(Negate, Num(1)), Binop(Times, Var("x"), App(Var("f"), 
				 Binop(Minus, Var("x"), Num(1)))))), App(Var("f"), Num(4))) ;; 

(* 
	let x = 2 in let f = (fun y -> x + y) in let x = 8 in f x ;; 
*)

let e3 = Let("x", Num(2), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))), 
				 Let("x", Num(8), App(Var("f"), Var("x"))))) ;;

(* 
	let x = 1. in 
	let f = fun y -> x + y in
	let x = 2. in
	f 3. ;; 
*)
let e4 = Let("x", Float(1.), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))),
		 Let("x", Float(2.), App(Var("f"), Float(3.))))) ;;

(* 
	let rec f = fun x -> if x = 0. then ~-1. else x * f (x - 1.) in f 4. ;;
*)
let e5 = Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Float(0.)), 
				 Unop(Negate, Float(1.)), Binop(Times, Var("x"), App(Var("f"), 
				 Binop(Minus, Var("x"), Float(1.)))))), App(Var("f"), Float(4.))) ;; 

(* 
	let x = 2. in let f = (fun y -> x + y) in let x = 8. in f x ;; 
*)

let e6 = Let("x", Float(2.), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))), 
				 Let("x", Float(8.), App(Var("f"), Var("x"))))) ;;


(* 
	let x = "seth" in 
	let f = (fun y -> x ^ " " ^ y) in 
	let x = "maddy" in 
	f "carolyn";; 
*)

let e7 = Let("x", String("seth"), Let("f", Fun("y", 
				 Binop(Concat, Binop(Concat, Var("x"), String(" ")), Var("y"))), 
				 Let("x", String("maddy"), App(Var("f"), String("carolyn"))))) ;;

let emptyenv = Env.create () ;; 

let eval_s_tests () =
	unit_test (extract_e (eval_s e1 emptyenv) = Num(4)) 
						"eval_s test ints Fun, let, App, Binop, Var, Num";
	unit_test (extract_e (eval_s e2 emptyenv) = Num(-24))
						"eval_s test ints Letrec, Conditional, Negate, Unop"; 
	unit_test (extract_e (eval_s e3 emptyenv) = Num(10))
						"eval_s test ints Let, Fun, Binop, App"; 
	unit_test (extract_e (eval_s e4 emptyenv) = Float(4.))
						"eval_s test floats Let, Fun, Binop, App"; 
	unit_test (extract_e (eval_s e5 emptyenv) = Float(-24.))
						"eval_s test floats Letrec, Conditional, Negate, Unop"; 
	unit_test (extract_e (eval_s e6 emptyenv) = Float(10.))
						"eval_s test floats Let, Fun, Binop, App"; 
	unit_test (extract_e (eval_s e7 emptyenv) = String("seth carolyn")) 
						"eval_l test strings, Concat" ;
	() ;;

let eval_d_tests () =
	unit_test (extract_e (eval_d e1 emptyenv) = Num(5)) 
						"eval_d test ints Fun, let, App, Binop, Var, Num";
	unit_test (extract_e (eval_d e2 emptyenv) = Num(-24))
						"eval_d test ints Letrec, Conditional, Negate, Unop"; 
	unit_test (extract_e (eval_d e3 emptyenv) = Num(16))
						"eval_d test ints Let, Fun, Binop, App"; 
	unit_test (extract_e (eval_d e4 emptyenv) = Float(5.)) 
						"eval_d test floats Fun, let, App, Binop, Var, Float";
	unit_test (extract_e (eval_d e5 emptyenv) = Float(-24.))
						"eval_d test floats Letrec, Conditional, Negate, Unop"; 
	unit_test (extract_e (eval_d e6 emptyenv) = Float(16.))
						"eval_d test floats Let, Fun, Binop, App"; 
	unit_test (extract_e (eval_d e7 emptyenv) = String("maddy carolyn")) 
						"eval_l test strings, Concat" ;
	() ;;

let eval_l_tests () =
	unit_test (extract_e (eval_l e1 emptyenv) = Num(4)) 
						"eval_l test ints Fun, let, App, Binop, Var, Num";
	unit_test (extract_e (eval_l e2 emptyenv) = Num(-24))
						"eval_l test ints Letrec, Conditional, Negate, Unop"; 
	unit_test (extract_e (eval_l e3 emptyenv) = Num(10))
						"eval_l test ints Let, Fun, Binop, App";
	unit_test (extract_e (eval_l e4 emptyenv) = Float(4.)) 
						"eval_l test floats Fun, let, App, Binop, Var, Float";
	unit_test (extract_e (eval_l e5 emptyenv) = Float(-24.))
						"eval_l test floats Letrec, Conditional, Negate, Unop"; 
	unit_test (extract_e (eval_l e6 emptyenv) = Float(10.))
						"eval_l test floats Let, Fun, Binop, App"; 
	unit_test (extract_e (eval_l e7 emptyenv) = String("seth carolyn")) 
						"eval_l test strings, Concat" ;
	() ;;

let _ = 
	subst_tests (); 
	eval_s_tests ();
	eval_d_tests ();
	eval_l_tests () ;;



