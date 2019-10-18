(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
  | Concat
;;

type varid = string ;;
  
type expr =
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
;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let rec free_vars (exp : expr) : varidset =
  let open SS in
  match exp with 
  | Var x -> singleton x
  | Num _ | Float _ | String _ | Bool _ | Raise | Unassigned -> empty
  | Unop (_, e) -> free_vars e 
  | Binop (_, e1, e2) -> union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) -> 
      union (free_vars e1) (union (free_vars e2) (free_vars e3))
  | Fun (var, e) -> remove var (free_vars e)
  | Let (var, p, q) -> union (remove var (free_vars q)) (free_vars p) 
  | Letrec (var, p, q) -> 
      union (remove var (free_vars q)) (remove var (free_vars p)) 
  | App (e1, e2) -> union (free_vars e1) (free_vars e2) ;;

  
(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname : unit -> varid =
  let ctr = ref 0 in
  fun () -> let v = "var" ^ string_of_int !ctr in
           ctr := !ctr + 1;
           v ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let open SS in 
  let rec sub (exp : expr) : expr =
    match exp with
    | Var x ->
        if x = var_name then repl else exp
    | Num _ | String _ | Float _ | Bool _ | Raise | Unassigned -> exp
    | Unop (op, arg) -> Unop (op, sub arg)
    | Binop (op, arg1, arg2) -> Binop (op, sub arg1, sub arg2)
    | Let (x, def, body) ->
        if x = var_name then Let(x, sub def, body) 
        else 
          if (mem x (free_vars repl)) 
            then let a = new_varname () in 
            Let (a, sub def, sub (subst x (Var(a)) body))
          else Let (x, sub def, sub body)
    | Conditional (e1, e2, e3) -> 
        Conditional (sub e1, sub e2, sub e3)
    | Fun (var, e) -> 
        if var = var_name then exp
        else if (mem var (free_vars repl)) 
             then let a = new_varname () in Fun (a, sub (subst var (Var(a)) e))
        else Fun (var, sub e) 
    | Letrec (var, p, q) -> 
        if var = var_name then exp
        (* else 
          if (mem var (free_vars repl)) 
          then let a = new_varname () in 
            Letrec (a, sub p, sub (subst var (Var(a)) q)) *)
        else Letrec (var, sub p, sub q) 
    | App (e1, e2) -> App (sub e1, sub e2)
  in
  sub exp ;;

(*......................................................................
  String representations of expressions
 *)
   
    
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
	let binop_to_c (b : binop) : string = 
		(match b with 
		 | Plus -> " + "
  	 | Minus -> " - "
  	 | Times -> " * "
  	 | Equals -> " = "
  	 | LessThan -> " <= "
     | Concat -> " ^ ") in 
  match exp with 
  | Var x -> x 
  | Num i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""
  | Bool b -> if b then "true" else "false"
  | Unop (_, e) -> "- " ^ "(" ^ exp_to_concrete_string e ^ ")"
  | Binop (b, e1, e2) -> exp_to_concrete_string e1 ^ binop_to_c b 
  											 ^ exp_to_concrete_string e2
  | Conditional (e1, e2, e3) -> "if " ^ exp_to_concrete_string e1 ^ " then " ^ 
  															exp_to_concrete_string e2 ^ " else "  ^
  															exp_to_concrete_string e3 
  | Fun (v, e) -> "fun " ^ v ^ " -> " ^ exp_to_concrete_string e
  | Let (v, e1, e2) -> 
  			"let " ^ v ^ " = " ^ exp_to_concrete_string e1 ^ " in " 
  			^ exp_to_concrete_string e2
  | Letrec (v, e1, e2) -> 
  			"let rec" ^ v ^ " = " ^ exp_to_concrete_string e1 ^ " in " 
  			^ exp_to_concrete_string e2   (* recursive local naming *)
  | Raise -> "raise Exception"                               (* exceptions *)
  | Unassigned -> "Unassigned"
  | App (e1, e2) -> 
      exp_to_concrete_string e1 ^ " " ^ exp_to_concrete_string e2 
;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  let binop_to_a (b : binop) : string = 
    (match b with 
     | Plus -> "Plus"
     | Minus -> "Minus"
     | Times -> "Times"
     | Equals -> "Equals"
     | LessThan -> "LessThan"
     | Concat -> " ^ ") in 
  let parenthesize x = 
    "(" ^ x ^ ")" in 
  let add1comma x1 x2 = 
   x1 ^ ", " ^ x2 in 
  let add2commas x1 x2 x3 = 
   x1 ^ ", " ^ x2 ^ ", " ^ x3 in 
  match exp with 
  | Var x -> "Var" ^ parenthesize x 
  | Num i -> "Num" ^ parenthesize (string_of_int i)
  | Float f -> "Float" ^ parenthesize (string_of_float f)
  | String s -> "String" ^ "(\"" ^ s ^ "\")"
  | Bool b -> 
     "Bool" ^ (if b then parenthesize "true" else parenthesize "false")
  | Unop (_, e) -> 
      "Unop" ^ parenthesize (add1comma "Negate" 
      (parenthesize (exp_to_abstract_string e)))
  | Binop (b, e1, e2) -> 
      "Binop" ^ parenthesize (add2commas (binop_to_a b) 
      (exp_to_abstract_string e1) (exp_to_abstract_string e2))
  | Conditional (e1, e2, e3) -> 
      "Conditional" ^ parenthesize (add2commas (exp_to_abstract_string e1) 
      (exp_to_abstract_string e2) (exp_to_abstract_string e3))
  | Fun (v, e) -> 
      "Fun" ^ parenthesize (add1comma v (exp_to_abstract_string e))
  | Let (v, e1, e2) -> 
        "Let" ^ parenthesize (add2commas v (exp_to_abstract_string e1)
        (exp_to_abstract_string e2))
  | Letrec (v, e1, e2) -> 
        "Letrec" ^ parenthesize (add2commas v (exp_to_abstract_string e1)
        (exp_to_abstract_string e2))
  | Raise -> "raise Exception"
  | Unassigned -> "Unassigned"
  | App (e1, e2) -> 
      "App" ^ parenthesize (add1comma (exp_to_abstract_string e1)
        (exp_to_abstract_string e2)) ;;
