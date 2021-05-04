(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)

open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error in
   the interpreter *)
exception EvalError of string ;;
  
(* Exception for evaluator runtime, generated by an explicit `raise`
   construct in the object language *)
exception EvalException ;;

(*......................................................................
  Environments and values 
 *)

module type ENV = sig
    (* the type of environments *)
    type env
    (* the type of values stored in environments *)
    type value =
      | Val of expr
      | Closure of (expr * env)
   
    (* empty () -- Returns an empty environment *)
    val empty : unit -> env

    (* close expr env -- Returns a closure for `expr` and its `env` *)
    val close : expr -> env -> value

    (* lookup env varid -- Returns the value in the `env` for the
       `varid`, raising an `Eval_error` if not found *)
    val lookup : env -> varid -> value

    (* extend env varid loc -- Returns a new environment just like
       `env` except that it maps the variable `varid` to the `value`
       stored at `loc`. This allows later changing the value, an
       ability used in the evaluation of `letrec`. To make good on
       this, extending an environment needs to preserve the previous
       bindings in a physical, not just structural, way. *)
    val extend : env -> varid -> value ref -> env

    (* env_to_string env -- Returns a printable string representation
       of environment `env` *)
    val env_to_string : env -> string
                                 
    (* value_to_string ?printenvp value -- Returns a printable string
       representation of a value; the optional flag `printenvp`
       (default: `true`) determines whether to include the environment
       in the string representation when called on a closure *)
    val value_to_string : ?printenvp:bool -> value -> string

    val iter : ((varid * value ref) -> unit) -> env -> unit
  end

module Env : ENV =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    let empty () : env = [] ;;

    let close (exp : expr) (env : env) : value =
      Closure (exp, env) ;;

    (* let lookup (env : env) (varname : varid) : value =
      let elt = List.assoc_opt varname env in
        if elt = None then raise (EvalError ("Undefined Variable: " ^ varname))
        else !(Option.get elt) ;; *)

    let rec lookup (env : env) (varname : varid) : value =
      match env with
      | [] -> raise (EvalError ("unbound variable: " ^ varname))
      | (var, value) :: tl -> if varname = var then !value
                              else lookup tl varname ;;

    

    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val exp -> Printf.sprintf "Val (%s)" (exp_to_abstract_string exp)
      | Closure (exp, envir) ->  if printenvp then 
                                      Printf.sprintf "Closure (%s, %s)"
                                      (exp_to_abstract_string exp)
                                      (env_to_string envir)
                                 else Printf.sprintf "Closure (%s, *ENV*)"
                                      (exp_to_abstract_string exp)

    and env_to_string (env : env) : string =
      match env with
      | [] -> ""
      | (v, v_value) :: tl -> Printf.sprintf "(%s, %s); %s"
                              v
                              (value_to_string !v_value)
                              (env_to_string tl) ;;

    let rec extend (env : env) (varname : varid) (loc : value ref) : env =
      let new_lst = List.remove_assoc varname env in
       (varname, loc) :: new_lst ;;

    let iter = List.iter
  end
;;


(*......................................................................
  Evaluation functions

  Each of the evaluation functions below evaluates an expression `exp`
  in an environment `env` returning a result of type `value`. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a `value` and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type `expr -> Env.env -> Env.value` for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). 

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as `eval_e` below.  *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   
let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

(* ======== helper functions ======= *)

let devalue (x : Env.value) : expr =
  match x with
  | Val (v) -> v
  | Closure (c) -> raise (EvalError "Attemp to De-value Closure")

let declosure (x : Env.value) : expr * Env.env =
  match x with
  | Val (v) -> raise (EvalError "Attemp to De-closure Value")
  | Closure (c) -> c

let value_to_int (e: Env.value) : int =
  match devalue e with
  | Num n -> n
  | _ -> raise (EvalError "Function Applied to Non-integer")

let value_to_bool (e: Env.value) : bool =
  match devalue e with
  | Bool b -> b
  | _ -> raise (EvalError "Non-boolean")

let eval_conditional (eval_fun : Expr.expr -> Env.value) 
                         (e1 : Expr.expr) 
                         (e2 : Expr.expr) 
                         (e3 : Expr.expr) : Env.value =
  let truth = value_to_bool (eval_fun e1) in
            if truth then eval_fun e2
            else eval_fun e3

let get_fun_args (f: Env.value) : Expr.varid * Expr.expr =
  match devalue f with
  | Fun (x,y) -> (x,y)
  | _ -> raise (EvalError "Function Error")

let unop_to_val (u : unop) (a : Env.value) : Env.value=
  let x = value_to_int a in
  match u with
  | Negate -> Val (Num (~- x)) ;;

let binop_to_val (b: binop) (p: Env.value) (q: Env.value) : Env.value =
  let x, y = value_to_int p, value_to_int q in
  match b with
  | Plus -> Env.Val (Num (x + y))
  | Minus -> Env.Val (Num (x - y))
  | Times -> Env.Val (Num (x * y))
  | Equals -> Env.Val (Bool (x = y))
  | LessThan -> Env.Val (Bool (x < y))

let replace_unassigned (env: Env.env) (repl: Env.value) : unit =
  Env.iter (fun elt -> if !(snd elt) = Env.Val Unassigned 
                        then (snd elt) := repl
                       else ()) env

(* ============================== *)

(* The SUBSTITUTION MODEL evaluator -- to be completed *)

let rec eval_s (exp : expr) (env : Env.env) : Env.value =
  let eval (e : expr) : Env.value = eval_s e env in
  match exp with
  | Var v -> raise (EvalError ("unbound variable: " ^ v))
  | Num i -> Env.Val (Num i)
  | Bool b -> Env.Val (Bool b)
  | Unop (un, exp1) -> unop_to_val un (eval exp1)
  | Binop (bi, exp1, exp2) -> binop_to_val bi (eval exp1) 
                                              (eval exp2)
  | Conditional (exp1, exp2, exp3) -> eval_conditional
                                       eval
                                       exp1 exp2 exp3
  | Fun (v, exp1) -> Env.Val (Fun (v, exp1))
  | Let (v, exp1, exp2) -> let sub = devalue (eval exp1) in
                            eval (subst v sub exp2)
  | Letrec (v, exp1, exp2) -> let vD = devalue (eval exp1) in
                              let repl = Letrec (v, vD, Var (v)) in
                              let new_repl = subst v repl vD in
                              eval (subst v new_repl exp2)
  | Raise
  | Unassigned -> raise EvalException
  | App (exp1, exp2) -> let x, b = get_fun_args (eval exp1) in
                          let sub = devalue (eval exp2) in
                            eval (subst x sub b) ;;
     
(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)
(* let rec env_eval (fun_eval) (rec_eval) (app_eval) (exp) (env) : Env.value =
  let eval e = env_eval fun_eval rec_eval app_eval e env in
  match exp with
  | Var v -> Env.lookup env v
  | Num i -> Env.Val (Num i)
  | Bool b -> Env.Val (Bool b)
  | Unop (un, exp1) -> unop_to_val un (eval exp1)
  | Binop (bi, exp1, exp2) -> binop_to_val bi (eval exp1) 
                                              (eval exp2)
  | Conditional (exp1, exp2, exp3) -> eval_conditional
                                       eval
                                       exp1 exp2 exp3
  | Fun (v, exp1) -> fun_eval v exp1 env
  | Let (v, exp1, exp2) -> let eval_exp1 = ref (eval exp1) in
                            let new_env = Env.extend env v eval_exp1 in
                              env_eval fun_eval rec_eval app_eval exp2 new_env 
  | Letrec (v, exp1, exp2) -> let eval_exp1 = ref (eval exp1) in
                              rec_eval v exp1 exp2
  | Raise
  | Unassigned -> raise EvalException
  | App (exp1, exp2) -> app_eval exp1 exp2;;

let fun_evald v exp _env : Env.value =
  Val (Fun (v, exp)) ;;
let fun_evall v exp env : Env.value =
  Closure (Fun (v, exp), env);;
let rec_evald =
  fun_evald ;;
let rec_;; *)


let rec eval_d (exp : expr) (env : Env.env) : Env.value =
  let eval e = eval_d e env in
  match exp with
  | Var v -> Env.lookup env v
  | Num i -> Env.Val (Num i)
  | Bool b -> Env.Val (Bool b)
  | Unop (un, exp1) -> unop_to_val un (eval exp1)
  | Binop (bi, exp1, exp2) -> binop_to_val bi (eval exp1) 
                                              (eval exp2)
  | Conditional (exp1, exp2, exp3) -> eval_conditional
                                       eval
                                       exp1 exp2 exp3
  | Fun (v, exp1) -> Val (Fun (v, exp1))
  | Letrec (v, exp1, exp2)
  | Let (v, exp1, exp2) -> let eval_exp1 = ref (eval exp1) in
                            let new_env = Env.extend env v eval_exp1 in
                              eval_d exp2 new_env
  | Raise
  | Unassigned -> raise EvalException
  | App (exp1, exp2) ->  let x, b = get_fun_args (eval exp1) in
                          let eval_exp2 = ref (eval exp2) in
                            let new_env = Env.extend env x eval_exp2 in
                            eval_d b new_env ;;
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)

let rec eval_l (exp : expr) (env : Env.env) : Env.value =
  let eval e = eval_l e env in
  match exp with
  | Var v -> Env.lookup env v
  | Num i -> Env.Val (Num i)
  | Bool b -> Env.Val (Bool b)
  | Unop (un, exp1) -> unop_to_val un (eval exp1)
  | Binop (bi, exp1, exp2) -> binop_to_val bi (eval exp1) 
                                              (eval exp2)
  | Conditional (exp1, exp2, exp3) -> eval_conditional
                                       eval
                                       exp1 exp2 exp3
  | Fun (v, exp1) -> Closure (Fun (v, exp1), env)
  | Let (v, exp1, exp2) -> let eval_exp1 = ref (eval exp1) in
                            let new_env = Env.extend env v eval_exp1 in
                              eval_l exp2 new_env
  | Letrec (v, exp1, exp2) -> 
      let new_env = Env.extend env v (ref (Env.Val Unassigned)) in
      let eval_exp1 = (eval_l exp1 new_env) in
        replace_unassigned new_env eval_exp1;
        eval_l exp2 new_env
  | Raise
  | Unassigned -> raise EvalException
  | App (p, q) -> match declosure(eval_l p env) with
                  | Fun (x, b), env_lexical -> 
                    let new_lexical = Env.extend env_lexical x (ref (eval q)) in
                      eval_l b new_lexical
                  | _ -> raise (EvalError "Eval_l Function Error") ;;

(* The EXTENDED evaluator -- if you want, you can provide your
   extension as a separate evaluator, or if it is type- and
   correctness-compatible with one of the above, you can incorporate
   your extensions within `eval_s`, `eval_d`, or `eval_l`. *)

let eval_e _ =
  failwith "eval_e not implemented" ;;
  
(* Connecting the evaluators to the external world. The REPL in
   `miniml.ml` uses a call to the single function `evaluate` defined
   here. Initially, `evaluate` is the trivial evaluator `eval_t`. But
   you can define it to use any of the other evaluators as you proceed
   to implement them. (We will directly unit test the four evaluators
   above, not the `evaluate` function, so it doesn't matter how it's
   set when you submit your solution.) *)

let evaluate_s = eval_s ;;
let evaluate_d = eval_d ;;
let evaluate_l = eval_l ;;
