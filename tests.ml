open Evaluation ;;
open Expr ;;
open Test_simple ;;

(* =========== EXPR TESTS =========== *)

let test () =
(* new_varname tests *)
    unit_test (new_varname () = "var0")
              "new_varname first" ;
    unit_test (new_varname () = "var1")
               "new_varname second" ;
    unit_test (new_varname () = "var2")
               "new_varname third";

(* free_vars tests *)
    unit_test (free_vars (Var "x") = vars_of_list ["x"])
              "free_vars Var";
    unit_test (free_vars (Unop (Negate, Num 2)) = vars_of_list [])
              "free_vars Unop";
    unit_test (same_vars (free_vars (Binop (Plus, Var "x", Var "y"))) 
              (vars_of_list ["x"; "y"]))
              "free_vars Binop";
    unit_test (same_vars (free_vars (Conditional (Var "x", Var "y", Var "z"))) 
              (vars_of_list ["x"; "y"; "z"]))
              "free_vars Conditional";
    unit_test (same_vars (free_vars (Fun ("x", Binop (Plus, Var "x", Var "y")))) 
              (vars_of_list ["y"]))
              "free_vars Fun";
    unit_test (same_vars (free_vars (Let ("x", Var "x", Var "y"))) 
              (vars_of_list ["y"]))
              "free_vars Let";
    unit_test (same_vars (free_vars (App (Binop (Plus, Var "x", Var "y"), Var "y"))) 
              (vars_of_list ["x"; "y"]))
              "free_vars App";

(* subst tests *)
    unit_test (subst "x" (Num 4) (Var "x") = Num 4)
              "subst Var" ;
    unit_test (subst "x" (Num 4) (Unop (Negate, Var "x")) = 
              Unop (Negate, Num 4))
              "subst Unop" ;
    unit_test (subst "x" (Num 4) (Binop (Plus, Var "x", Var "y")) = 
              Binop (Plus, Num 4, Var "y"))
              "subst Binop" ;
              
    unit_test (subst "x" (Num 4) (Fun ("f", Binop (Plus, Var "x", Var "y"))) = 
              (Fun ("f", Binop (Plus, Num 4, Var "y"))))
              "subst Fun Type 1" ;
    unit_test (subst "x" (Num 4) (Fun ("x", Binop (Plus, Var "x", Var "y"))) = 
              (Fun ("x", Binop (Plus, Var "x", Var "y"))))
              "subst Fun Type 2" ;
    unit_test (subst "x" (Var "f") (Fun ("f", Binop (Plus, Var "x", Var "y"))) = 
              (Fun ("var0", Binop (Plus, Var "f", Var "var0"))))
              "subst Fun Type 3" ;
    
    unit_test (subst "x" (Num 4) (Fun ("f", Binop (Plus, Var "x", Var "y"))) = 
              (Fun ("f", Binop (Plus, Num 4, Var "y"))))
              "subst Let Type 1" ;
    unit_test (subst "x" (Num 4) (Fun ("x", Binop (Plus, Var "x", Var "y"))) = 
              (Fun ("x", Binop (Plus, Var "x", Var "y"))))
              "subst Let Type 2" ;
    unit_test (subst "x" (Var "f") (Fun ("f", Binop (Plus, Var "x", Var "y"))) = 
              (Fun ("var0", Binop (Plus, Var "f", Var "var0"))))
              "subst Let Type 3" ;

(* exp_to_concrete_string tests*)
    (* TESTED IN UTOP *)

(* exp_to_abstract_string tests *)
    (* TESTED IN UTOP *)

(* =========== EVALUATION TESTS =========== *)

(* Env.lookup tests *)

(* Env.extend tests *)

(* Eval_s tests *)

(* Eval_d tests *)

(* Eval_l tests *)
;;

let _ = test () ;;