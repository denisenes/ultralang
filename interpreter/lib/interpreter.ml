open Parser
open Printer
open Common

let rec eval_sexpression sexpr env =
  match sexpr with
    | Boolean(v) -> (Boolean(v), env)
    | Fixnum(v)  -> (Fixnum(v) , env)
    | Symbol(v)  -> (Symbol(v), env)
    | Nil        -> (Nil, env)
    | Pair(Symbol("if"), Pair(cond, Pair(iftrue, Pair(iffalse, Nil)))) ->
      eval_sexpression (eval_if_expr cond iftrue iffalse env) env
    | _ -> raise (EvaluationError("cannot evaluate s-expression"))

and eval_if_expr cond iftrue iffalse env =
  let (condval, _) = eval_sexpression cond env in
  match condval with
    | Boolean(true)  -> iftrue
    | Boolean(false) -> iffalse
    | _ -> raise (EvaluationError("Wrong condition expression"))

let rec loop stream env = 
  print_string "> ";
  flush stdout;
  (* Read *)
  let expr = read_sexpression stream in
  (* Eval *)
  let (res, env') = eval_sexpression expr env in
  (* Print *)
  print_sexpression res;
  print_newline ();
  (* Loop *)
  loop stream env';;

let repl = 
  let stream = { buffer=[]; line_num=0; in_chan=stdin} in
  loop stream Nil