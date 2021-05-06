open Evaluation ;;
open Expr ;;
open Test_simple ;;
open Miniml;;

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

let env_test1 = Env.extend (Env.empty ()) "x" (ref (Env.Val (Num 2))) in
let env_test2 = Env.extend env_test1 "y" (ref (Env.Val (Fun ("x", Var "x")))) in

(* Env.lookup\extend tests *)
    unit_test (Env.lookup env_test1 "x" = Env.Val (Num 2))
              "lookup/extend basic" ;
    unit_test (Env.lookup env_test2 "y" = Env.Val (Fun ("x", Var "x")))
              "lookup/extend fun" ;

let eval_test1 = Let("f", Fun("x", Fun("z", Binop(Plus, Var("z"), 
                 Var("x")))), App(App(Var("f"), Num(4)), Num(5))) in
let eval_test2 = Let("x", Num(2), Let("f", Fun("y", Binop(Times, Var("x"), 
                 Var("y"))), Let("x", Num(1), App(Var("f"), Num(21))))) in
let eval_test3 = Let("f", Fun("x", Var("x")), App(Var("f"), Num(1))) in
let eval_test4 = Letrec("f", Fun("n", Conditional(Binop(Equals, Var("n"), Num(0)),
                 Num(1), Binop(Times, Var("n"), App(Var("f"), Binop(Minus, Var("n"), 
                 Num(1)))))), App(Var("f"), Num(2))) in
(* Eval_s tests *)
    unit_test (eval_s eval_test3 (Env.empty ()) = Env.Val (Num 1))
              "eval_s easy" ;
    unit_test (eval_s eval_test1 (Env.empty ()) = Env.Val (Num 9))
              "eval_s medium";
    unit_test (eval_s eval_test4 (Env.empty ()) = Env.Val (Num 2))
              "eval_s letrec" ;
    unit_test (eval_s eval_test2 (Env.empty ()) = Env.Val (Num 42))
              "eval_s vs dynamic" ;
(* Eval_d tests *)
    unit_test (eval_d eval_test3 (Env.empty ()) = Env.Val (Num 1))
              "eval_d easy" ;
    unit_test (eval_d eval_test4 (Env.empty ()) = Env.Val (Num 2))
              "eval_d letrec" ;
    unit_test (eval_d eval_test2 (Env.empty ()) = Env.Val (Num 21))
              "eval_d vs lex/sub" ;
(* Eval_l tests *)
    unit_test (eval_l eval_test3 (Env.empty ()) = Env.Val (Num 1))
              "eval_l easy" ;
    unit_test (eval_l eval_test1 (Env.empty ()) = Env.Val (Num 9))
              "eval_l medium";
    unit_test (eval_l eval_test4 (Env.empty ()) = Env.Val (Num 2))
              "eval_l letrec" ;
    unit_test (eval_l eval_test2 (Env.empty ()) = Env.Val (Num 42))
              "eval_l vs dynamic" ;
;;

let _ = test () ;;