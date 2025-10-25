let newline ='\n'
let whitespaces =  [newline; ' '; '\t'; '\r']
let infix_op_char = ['*'; '+'; '-'; '/'; '>'; '<'; '='; '|'; '&'; ':']
let delimiters = ['('; ')'; '{'; '}'; '['; ']'; ';'; '\"'; ',']
let keywords = [
  "module"; "begin"; "end";
  "if"; "then"; "else"; 
  "cond"; "endcond"; "=>"; "="; "|"; 
  "const"; "fn"; "in"; "let";
]


let is_white c   = List.mem c whitespaces
let is_delim c   = List.mem c delimiters || List.mem c whitespaces
let is_keyword c = List.mem c keywords

let is_digit c =
  let code = Char.code c in 
    code >= Char.code('0') && code <= Char.code('9')


type identifier_kind =
  | Lower
  | Upper


let is_lower_identifier_start = function 
    | 'a'..'z' | '_' -> true
    | _ -> false


let is_lower_identifier_char = function
  | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> true
  | _ -> false


let is_upper_identifier_start = function 
    | 'A'..'Z' -> true
    | _ -> false


let is_upper_identifier_char = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false