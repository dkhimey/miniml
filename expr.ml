(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

(* match exp with
  | Var v ->
  | Num i ->
  | Bool b ->
  | Unop (un, exp1) ->
  | Binop (bi, exp1, exp2) ->
  | Conditional (exp1, exp2, exp3) ->
  | Fun (v, exp1) ->
  | Let (v, exp1, exp2) ->
  | Letrec (v, exp1, exp2) ->
  | Raise ->
  | Unassigned ->
  | App (exp1, exp2) ->  *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
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
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let free_vars (exp : expr) : varidset =
  let rec add_to_set (e: expr) (set: varidset) =
    match e with
    | Var v -> SS.add v set
    | Raise
    | Unassigned
    | Num _
    | Bool _ -> set
    | Unop (un, exp1) -> add_to_set exp1 set
    | Binop (bi, exp1, exp2) -> add_to_set exp1 (add_to_set exp2 set)
    (* check this one *)
    | Conditional (exp1, exp2, exp3) -> add_to_set exp1 
                                        (add_to_set exp2 
                                        (add_to_set exp3 set))

    | Fun (v, exp1) -> add_to_set exp1 (SS.remove v set)
    | Let (v, exp1, exp2)
    (* make sure letrec and let are the same *)
    | Letrec (v, exp1, exp2) -> add_to_set exp1 (add_to_set exp2 (SS.remove v set))
    | App (exp1, exp2) -> add_to_set exp1 (add_to_set exp2 set) in
  add_to_set exp SS.empty;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)

(* Are we allowed to change header line?
    let new_varname () : varid = *)

let new_varname =
    let ctr = ref 0 in
    fun () ->
      let temp = "x" ^ string_of_int !ctr in
      incr ctr;
      temp ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)

   (* 1. is the subbing var same as input? if true then done 
      2. is the input of func in the free vars of expression we're subbing?
          --> then two different cases*)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let frees = free_vars exp in
  match exp with
  | Var v -> if SS.mem v frees && v = var_name then repl
             else Var v
  | Num i -> Num i
  | Bool b -> Bool b
  | Unop (un, exp1) ->  Unop (un, subst var_name repl exp1)
  | Binop (bi, exp1, exp2) ->  Binop (bi, subst var_name repl exp1, 
                                          subst var_name repl exp2)
  (* check conditional *)
  | Conditional (exp1, exp2, exp3) -> Conditional (subst var_name repl exp1, 
                                                   subst var_name repl exp2, 
                                                   subst var_name repl exp3)
  | Fun (v, exp1) -> 
                    if var_name = v then Fun (v, exp1)
                    else let repl_frees = free_vars repl in
                         if SS.mem v repl_frees then 
                          let z = new_varname () in
                            Fun (z, subst var_name repl (subst v (Var z) exp1))
                         else Fun (v, subst var_name repl exp1)
  | Let (v, exp1, exp2) -> if v = var_name then 
                              Let (v, subst var_name repl exp1, exp2)
                           else let repl_frees = free_vars repl in
                                if SS.mem v repl_frees then 
                                  let z = new_varname () in
                                    Let (z,
                                        subst var_name repl exp1,
                                        subst var_name repl (subst v (Var z) exp2))
                                else Let (v,
                                         subst var_name repl exp1,
                                         subst var_name repl exp2)
  (* is letrec the same as let? *)
  | Letrec (v, exp1, exp2) -> failwith "not done"
  | Raise -> Raise
  | Unassigned -> Unassigned
  | App (exp1, exp2) -> App (subst var_name repl exp1, subst var_name repl exp2);;

(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)

let unop_to_concrete_string (u: unop) : string =
  match u with
  | Negate -> "Negate" ;;

let binop_to_concrete_string (b: binop) : string =
  match b with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Equals -> "="
  | LessThan -> "<" ;;

let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var v -> Printf.sprintf "%s" v
  | Num i -> Printf.sprintf "%i" i
  | Bool b -> Printf.sprintf "%b" b
  | Unop (un, exp1) -> let s = unop_to_concrete_string un in
                      Printf.sprintf "%s %s" s (exp_to_concrete_string exp1)
  | Binop (bi, exp1, exp2) -> let s = binop_to_concrete_string bi in
                              Printf.sprintf "%s %s %s"
                              (exp_to_concrete_string exp1)
                              s
                              (exp_to_concrete_string exp2)
  | Conditional (exp1, exp2, exp3) -> Printf.sprintf "if (%s) then (%s) else (%s)"
                                      (exp_to_concrete_string exp1)
                                      (exp_to_concrete_string exp2)
                                      (exp_to_concrete_string exp3)
  | Fun (v, exp1) -> Printf.sprintf "fun %s -> (%s)" v
                     (exp_to_concrete_string exp1)
  | Let (v, exp1, exp2) -> Printf.sprintf "let %s = %s in %s"
                           v
                           (exp_to_concrete_string exp1)
                           (exp_to_concrete_string exp2)
  | Letrec (v, exp1, exp2) -> Printf.sprintf "let rec %s = (%s) in (%s)"
                              v
                              (exp_to_concrete_string exp1)
                              (exp_to_concrete_string exp2)
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (exp1, exp2) -> Printf.sprintf "(%s %s)" 
                       (exp_to_concrete_string exp1)
                       (exp_to_concrete_string exp2) ;;

     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)

let unop_to_abstract_string (u: unop) : string =
  match u with
  | Negate -> "Negate" ;;

let binop_to_abstract_string (b: binop) : string =
  match b with
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Equals -> "Equals"
  | LessThan -> "Less Than" ;;

let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var v -> Printf.sprintf "Var(%s)" v
  | Num i -> Printf.sprintf "Num(%i)" i
  | Bool b -> Printf.sprintf "Bool(%b)" b
  | Unop (un, exp1) -> let s = unop_to_abstract_string un in
                      Printf.sprintf "Unop(%s, %s)" s (exp_to_abstract_string exp1)
  | Binop (bi, exp1, exp2) -> let s = binop_to_abstract_string bi in
                              Printf.sprintf "Binop(%s, %s, %s)" s 
                              (exp_to_abstract_string exp1)
                              (exp_to_abstract_string exp2)
  | Conditional (exp1, exp2, exp3) -> Printf.sprintf "Conditional(%s, %s, %s)"
                                      (exp_to_abstract_string exp1)
                                      (exp_to_abstract_string exp2)
                                      (exp_to_abstract_string exp3)
  | Fun (v, exp1) -> Printf.sprintf "Fun(%s, %s)" v
                     (exp_to_abstract_string exp1)
  | Let (v, exp1, exp2) -> Printf.sprintf "Let(%s, %s, %s)"
                           v
                           (exp_to_abstract_string exp1)
                           (exp_to_abstract_string exp2)
  | Letrec (v, exp1, exp2) -> Printf.sprintf "Letrec(%s, %s, %s)"
                              v
                              (exp_to_abstract_string exp1)
                              (exp_to_abstract_string exp2)
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (exp1, exp2) -> Printf.sprintf "App(%s, %s)" 
                       (exp_to_abstract_string exp1)
                       (exp_to_abstract_string exp2) ;;
