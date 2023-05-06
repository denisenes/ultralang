open Common

let whitespaces =  ['\n'; ' '; '\t'; '\r']
let spec_symbols = ['*'; '+'; '-'; '/'; '>'; '<'; '='; '?'; '!'; '-'; '+'; '\'']
let delimiters = ['('; ')'; '{'; '}'; ';'; '\"']

let is_white c = List.mem c whitespaces
let is_delim c = List.mem c delimiters || List.mem c whitespaces

let string_of_char c = String.make 1 c;;

let is_digit c =
  let code = Char.code c in 
    code >= Char.code('0') && code <= Char.code('9');;

let is_sym_forwarding_chr c =
  let is_alpha = function 
    | 'A'..'Z' | 'a'..'z' -> true
    | _ -> false in
  if List.mem c spec_symbols
    then true
    else is_alpha c

let rec is_list pair =
  match pair with
    | Nil -> true
    | Pair(_ , cdr) -> is_list cdr
    | _ -> false