open Common

let whitespaces =  ['\n'; ' '; '\t'; '\r']
let spec_symbols = ['*'; '+'; '-'; '/'; '>'; '<'; '!'; '-'; '+'; '\'']
let delimiters = ['('; ')'; '{'; '}'; '['; ']'; ';'; '\"'; ',']

let keywords = 
  ["if"; "then"; "else"; "cond"; "=>"; "|"; "="; "val"; "fn";
  "in"; "let"; "nothing"]

let is_white c = List.mem c whitespaces
let is_delim c = List.mem c delimiters || List.mem c whitespaces
let is_keyword c = List.mem c keywords

let string_of_char c = String.make 1 c;;

let is_digit c =
  let code = Char.code c in 
    code >= Char.code('0') && code <= Char.code('9');;

let is_ident_forwarding_chr = function 
    | 'A'..'Z' | 'a'..'z' | '_' -> true
    | _ -> false

let is_ident_valid_chr = function
  | 'A' .. 'Z' | 'a' .. 'z' 
  | '0' .. '9' | '_' | '?' -> true
  | _ -> false

let rec is_list pair =
  match pair with
    | Nil -> true
    | Pair(_ , cdr) -> is_list cdr
    | _ -> false

(* (Pair x1 (Pair x2 ...)) --> [x1, x2, ...] *)
let list_of_pairs (sexpr : value) : value list = 
  let rec list_of_pairs' sexpr acc =
    match sexpr with
      | Nil -> List.rev acc
      | Pair(car, cdr) -> list_of_pairs' cdr (car :: acc)
      | _ -> raise (EvaluationError("Cannot transform Pair to list"))
  in
  list_of_pairs' sexpr []

let pretty_bool = function
  | true  -> "true"
  | false -> "false"

(* Couldn't find something like that in stdlib :( *)
let from_opt (default : 'a) (opt : 'a option) : 'a =
  match opt with
  | None   -> default
  | Some v -> v


module UList = struct

  let rec take n lst = 
    match lst with
    | [] when n > 0 -> raise (Failure "Take expected longer list")
    | [] -> []
    | (x::xs) -> (
      match n with
      | 0 -> []
      | _ -> x :: take (n-1) xs)

  let unzip (pairs : ('a * 'b) list) : 'a list * 'b list =
    List.fold_right 
      (fun (x, y) (xs, ys) -> (x::xs, y::ys))
      pairs ([], [])

end


module ExtSyntax = struct

  type    bool_exp = BoolExp of bool
  type 'a case_exp = Case    of bool_exp * 'a 

  let rec cond (exps : 'a case_exp list) =
    match exps with
    | [] -> raise @@ InternalError ("Cond error")
    | Case (BoolExp bexp, exp)::tail -> if bexp then exp else cond tail

  let otherwise : bool_exp = BoolExp true
  let case (exp : bool) : bool_exp = BoolExp exp
  let ( => ) (exp1 : bool_exp) (exp2 : 'a) = Case (exp1, exp2)

end