(* 
                         CS 51 Final Project
                    MiniML -- Read-Eval-Print Loop
 *)

module Ev = Evaluation ;;
module MP = Miniml_parse ;;
module ML = Miniml_lex ;;
module Ex = Expr ;;

open Printf ;;

(* str_to_exp str -- Returns the expression specified by `str` using
   the MiniML parser. *)
let str_to_exp (str: string) : Ex.expr =
  let lexbuf = Lexing.from_string str in
  let exp = MP.input ML.token lexbuf in
  exp ;;

(* repl () -- Read-eval-print loop for MiniML, which prompts for and
   evaluates MiniML expressions, printing the resulting value. Exits
   the loop and terminates upon reading an end-of-file
   (control-d). *)
let repl () =
  (* lexical analyzer buffer from stdin *)
  let lexbuf = Lexing.from_channel stdin in
  (* set up the initial environment *)
  let env = Ev.Env.empty () in

  (* the main LOOP *)
  while true do
  (* prompt *)
  printf "<== %!";
  (* READ and parse an expression from the input *)
  let exp = MP.input ML.token lexbuf in 

  printf "%s\n" (Expr.exp_to_abstract_string exp);
  
    (try
        (* EVALuate it *)
        let res_s = Ev.evaluate_s exp env in
        (* PRINT the result; in this initial version, the trivial
           evaluator just returns the expression unchanged as an
           element of the `Env.value` type (found in `expr.ml`), so we
           just extract the expression back out and print it *)
        (match res_s with
        | Val resexp ->
           printf "s ==> %s\n" (Ex.exp_to_concrete_string resexp)
        | _ -> failwith "not handling other cases yet");
      with
      | MP.Error -> printf "xx> parse error\n"
      | Ev.EvalError msg -> printf "sx> evaluation error: %s\n" msg
      | Ev.EvalException -> printf "sx> evaluation exception\n"
      | End_of_file -> printf "Goodbye.\n"; exit 0
    );

    (try
        let res_d = Ev.evaluate_d exp env in
        (match res_d with
        | Val resexp ->
           printf "d ==> %s\n" (Ex.exp_to_concrete_string resexp)
        | _ -> failwith "not handling other cases yet");

      with
      | MP.Error -> printf "xx> parse error\n"
      | Ev.EvalError msg -> printf "dx> evaluation error: %s\n" msg
      | Ev.EvalException -> printf "dx> evaluation exception\n"
      | End_of_file -> printf "Goodbye.\n"; exit 0
    );

    (try
        let res_l = Ev.evaluate_l exp env in
        (match res_l with
        | Val resexp ->
           printf "l ==> %s\n" (Ex.exp_to_concrete_string resexp)
        | _ -> failwith "not handling other cases yet");

      with
      | MP.Error -> printf "xx> parse error\n"
      | Ev.EvalError msg -> printf "lx> evaluation error: %s\n" msg
      | Ev.EvalException -> printf "lx> evaluation exception\n"
      | End_of_file -> printf "Goodbye.\n"; exit 0
    );
    flush stdout
  done
;;
        
(* Run REPL if called from command line *)

try
  let _ = Str.search_forward (Str.regexp "miniml\\.\\(byte\\|native\\|bc\\|exe\\)")
                             (Sys.argv.(0)) 0 in
  repl ()
with Not_found -> () ;;
