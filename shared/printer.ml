open Common
open Utils

let dbg_mode = false

let is_printable_value = function
  | Fixnum _ -> true
  | Boolean _ -> true
  | Symbol  _ -> true
  | Pair _ -> true
  | Primitive _ -> dbg_mode
  | Closure _ -> dbg_mode
  | Quote _ -> true
  | Nil -> true


let rec ast_to_string (node : exp) (lvl : int) : string =
  let rec tab_helper l =
    if l == 0 then "" else "\t" ^ tab_helper (l-1)
  in

  let tab l = "\n" ^ tab_helper l in

  match node with
  | Literal e -> 
    (tab lvl) ^ "Lit(" ^ value_to_string e ^ ")"
  | Var name ->  
    (tab lvl) ^ "Var(" ^ name ^ ")"
  | If(cond, l, r) ->
    (tab lvl) ^ "(Node<If>" ^ ast_to_string cond (lvl+1) ^ ast_to_string l (lvl+1) ^ ast_to_string r (lvl+1) ^ (tab lvl) ^ ")"
  | And(val1, val2) -> 
    (tab lvl) ^ "(Node<And>" ^ ast_to_string val1 (lvl+1) ^ ast_to_string val2 (lvl+1) ^ (tab lvl) ^ ")"
  | Or(val1, val2) -> 
    (tab lvl) ^ "(Node<Or>" ^ ast_to_string val1 (lvl+1) ^ ast_to_string val2 (lvl+1) ^ (tab lvl) ^ ")"
  | Apply(func, e) -> 
    (tab lvl) ^ "(Node<Apply>" ^ ast_to_string func (lvl+1) ^ ast_to_string e (lvl+1) ^ (tab lvl) ^ ")"
  | Call(func, args) -> 
    let string_args = (String.concat " " (List.map (fun x -> ast_to_string x (lvl+1)) args)) in
    (tab lvl) ^ "(Node<Call>" ^ ast_to_string func (lvl+1) ^ string_args ^ (tab lvl) ^ ")"
  | Lambda (args, body) -> 
    (tab lvl) ^ "(Node<Lambda> " ^ args_to_string args ^ ast_to_string body (lvl+1) ^ (tab lvl) ^ ")"
  | Let (var_name, var_val, body) -> 
    (tab lvl) ^ "(Node<Let> " ^ var_name ^ ast_to_string var_val (lvl+1) ^ ast_to_string body (lvl+1) ^ (tab lvl) ^  ")"
  | Defexp(Val(name, e)) -> 
    (tab lvl) ^ "(Node<Val> " ^ name ^ ast_to_string e (lvl+1) ^ (tab lvl) ^ ")"
  | Defexp(Exp exp) -> (tab lvl) ^ ast_to_string exp (lvl+1)
  | Defexp(FnDef(name, arg_names, body)) ->
    (tab lvl) ^ "(Node<FnDef> " ^ name ^ " " ^ args_to_string arg_names ^ ast_to_string body (lvl+1) ^ (tab lvl) ^ ")"

and value_to_string (sexpr : value) : string =
  match sexpr with
    | Fixnum(value)  -> string_of_int value
    | Boolean(b) -> if b then "#t" else "#f"
    | Symbol(name) -> name
    | Pair(_ , _) -> 
      "(" ^
      (if is_list sexpr then list_to_string sexpr else pair_to_string sexpr)
      ^ ")"
    | Closure (args, _, _) -> "<closure>(" ^ args_to_string args ^ ")" 
    | Primitive (name,  _) -> "<" ^ name ^ ">"
    | Quote datum -> "\'" ^ value_to_string datum
    | Nil -> "nil"

and list_to_string (list : value) : string = 
  match list with
    | Pair(car, Nil) -> value_to_string car
    | Pair(car, cdr) -> value_to_string car ^ " " ^ list_to_string cdr
    | _ -> raise (PrintError("Something went wrong while printing list"))

and args_to_string (args : string list) : string =
  List.fold_left (fun arg acc -> acc ^ " " ^ arg) "" args

and pair_to_string pair = match pair with
  | Pair (car, cdr) -> value_to_string car ^ " . " ^ value_to_string cdr
  | _ -> raise (PrintError("Something went wrong while printing pair"))

let print_value (sexpr : value) : unit =
  print_endline (value_to_string sexpr)

let rec print_env (e : value env) : unit = match e with
  | [] -> ()
  | ((name, value_or_none)::rest) -> 
    (match !value_or_none with
      | Some value -> print_endline (name ^ " " ^ (value_to_string value))
      | None -> print_string "<nothing>")
  ; (print_env rest)