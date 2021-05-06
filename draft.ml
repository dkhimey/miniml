(* let rec eval_l (exp : expr) (env : Env.env) : Env.value =
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
                  | _ -> raise (EvalError "Eval_l Function Error") ;; *)

(* let rec eval_d (exp : expr) (env : Env.env) : Env.value =
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
                            eval_d b new_env ;; *)