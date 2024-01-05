open Common

let rec lower_cond subexps =
  match subexps with
  | [] -> ShouldNotReachHere 0
  | (condition, branch)::tail -> 
    If (lower_ast condition, lower_ast branch, lower_cond tail)

and lower_list_literal exps =
  match exps with
  | [] -> Literal Nil
  | (exp::tail) -> Call (Ident ":", [lower_ast exp; lower_list_literal tail])

and lower_ast = function
  (* Lowered nodes *)
  | Call (Ident "apply", args) ->
    Apply (lower_ast @@ List.hd args, List.map lower_ast (List.tl args))
  | Cond subexps     -> lower_cond subexps
  | ListLiteral exps -> lower_list_literal exps
  (* Trivial cases*)
  | Literal _            as node -> node
  | Ident _              as node -> node
  | ShouldNotReachHere _ as node -> node
  | If (cond, exp1, exp2)  ->
    If (lower_ast cond, lower_ast exp1, lower_ast exp2)
  | Let (name, exp1, exp2) ->
    Let (name, lower_ast exp1, lower_ast exp2)
  | Apply (funcexp, args)  ->
    Apply (lower_ast funcexp, List.map lower_ast args)
  | Call (funcexp, args)   ->
    Call (lower_ast funcexp, List.map lower_ast args)
  | Lambda (argnames, exp) ->
    Lambda (argnames, lower_ast exp)

let lower_hl_entry = function
  | DefVal (name, exp) -> DefVal (name, lower_ast exp)
  | DefFn  (name, args, body) -> DefFn (name, args, lower_ast body)
  | HLExp exp -> HLExp (lower_ast exp)