open Common
open Utils

let rec value_to_string (sexpr : value) : string =
  match sexpr with
    | Fixnum(value)  -> string_of_int value
    | Boolean(b) -> if b then "#t" else "#f"
    | Symbol(name) -> name
    | Pair(_ , _) -> 
      "(" ^
      (if is_list sexpr then list_to_string sexpr else pair_to_string sexpr)
      ^ ")"

    | Primitive (name,  _) -> "<" ^ name ^ ">"
    | Quote datum -> "\'" ^ value_to_string datum
    | Nil -> "nil"

and list_to_string (list : value) : string = 
  match list with
    | Pair(car, Nil) -> value_to_string car
    | Pair(car, cdr) -> value_to_string car ^ " " ^ list_to_string cdr
    | _ -> raise (PrintError("Something went wrong while printing list"))

and pair_to_string pair = match pair with
  | Pair (car, cdr) -> value_to_string car ^ " . " ^ value_to_string cdr
  | _ -> raise (PrintError("Something went wrong while printing pair"))

let print_value (sexpr : value) : unit =
  print_endline (value_to_string sexpr)