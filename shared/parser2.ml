open Common
open Utility

type string_reader =
  {
    mutable sym_idx : int;
    buffer          : string
  };;
  
let constr_string_reader (s : string) : string_reader =
  { sym_idx=0 ; buffer = s ^ "\n" }

let sr_get_symbol (sr : string_reader) : char =
  let sym = String.get sr.buffer sr.sym_idx in
  sr.sym_idx <- sr.sym_idx + 1;
  sym

let sr_peek (sr : string_reader) : char =
  if sr.sym_idx >= String.length sr.buffer
    then raise End_of_file
    else String.get sr.buffer sr.sym_idx

type input_source = Chan of in_channel | String of string_reader 

type input_stream =
  {
    mutable line_num : int; 
    mutable buffer   : char list; 
    source           : input_source 
  };;

let new_input_stream (sr : string_reader) =
  {buffer=[]; line_num=0; source=(String sr)}

(* peek next character without stream modification *)
let peek_char stream =
  match stream.buffer with
    | [] ->
      let c = begin match stream.source with
        | Chan _ -> ' ' (* TODO: peek from channel *)
        | String sr -> sr_peek sr 
      end in c
    | c::_ -> c

let eof_in_stream stream =
  try
    let _ = peek_char stream in false
  with
    End_of_file -> true

(* read one character from input stream *)
let read_char stream =
  match stream.buffer with
    | [] ->
      let c = begin match stream.source with
        | Chan channel -> input_char channel
        | String sr    -> sr_get_symbol sr
      end
      in
      if c = '\n' 
        then let _ = stream.line_num <- stream.line_num + 1 in c
        else c
    | c::rest -> 
      let _ = stream.buffer <- rest in c

(* place charachter back to stream buffer *)
let unread_char stream c = 
  stream.buffer <- c :: stream.buffer;;

let rec read_token stream read_delim : string =
  let rec read_token' (tok : string) =
    let ch = read_char stream in
    if is_white ch || (is_delim ch && not read_delim) then begin 
      unread_char stream ch;
      tok
    end else
      read_token' (tok ^ string_of_char ch)
  in
  eat_whitespaces stream;
  let s = (read_token' "") in
  Printf.printf "(Token read: %s)\n" s;
  s

and unread_token stream (tok : string) : unit =
  Printf.printf "(Token unread: %s)\n" tok;
  if tok = "" then () else
  let tok' = tok ^ " " in (* because we don't want to join this token with the next *)
  let chs = List.init (String.length tok') (String.get tok') in
  List.fold_right (fun c _ -> unread_char stream c; ()) chs ()

(* remove leading whitespaces in stream *)
and eat_whitespaces stream =
  if eof_in_stream stream 
    then () 
  else
    
  let chr = read_char stream in
  if is_white chr
    then eat_whitespaces stream 
    else unread_char stream chr;
  ();;

(* === Parser === *)

let check_next_keyword stream expected =
  let actual = read_token stream false in
  if actual = expected && is_keyword actual
    then ()
    else raise (SyntaxError ("Unexpected token: expected = " ^ expected ^ ", actual = " ^ actual))

let is_valid_ident ident =
  if not (is_keyword ident) then begin 
    let fch = String.get ident 0 in
    (is_ident_forwarding_chr fch) && 
    String.for_all (fun c -> is_ident_valid_chr c) ident
  end else
    false

let check_is_valid_ident (ident : string) : unit =
  if (is_valid_ident ident)
    then ()
    else raise (SyntaxError "Incorrect identifier")

let check_opening_delim stream del : bool =
  let opening = read_char stream in
  match opening with
  | '(' | '[' when opening = del -> true
  | _ -> begin
    unread_char stream opening; 
    false
  end 

let check_closing_delim stream del =
  let closing = read_char stream in
  match closing with
  | ')' | ']' when closing = del -> ()
  | _ -> raise (SyntaxError ("Unexpected ending of an expression: " ^ string_of_char closing))

let rec parse_call_args stream (acc : c_exp list) : c_exp list =
  if not (List.is_empty acc) then begin
    eat_whitespaces stream;
    let ch = read_char stream in
    match ch with
      | ',' -> parse_call_args stream (acc @ [parse_infix_exp stream]) 
      | ')' -> unread_char stream ch; acc
      | _ -> raise (SyntaxError "Expected delimeter between call arguments") 
  end else (* Parse first argument *)
    let tok = read_token stream false in
    match tok with
    | "nothing" -> begin
      let ch = read_char stream in
      match ch with
      | ')' -> unread_char stream ch; []
      | _ -> raise (SyntaxError "Incorrect syntax of 0-ary function")
      end
    | _ -> begin
      unread_token stream tok;
      parse_call_args stream (acc @ [parse_infix_exp stream])
      end

and parse_cond_subexps stream acc : (c_exp * c_exp) list =
  let parse_subexp () = 
    let bool_exp   = parse_infix_exp stream in
    check_next_keyword stream "=>";
    let branch_exp = parse_infix_exp stream in
    (bool_exp, branch_exp)
  in

  eat_whitespaces stream;
  let next_tok = read_token stream false in
  match next_tok with
  | "endcond" -> List.rev acc
  | "|" -> 
    let subexp = parse_subexp () in
    parse_cond_subexps stream (subexp::acc)
  | _ when List.is_empty acc -> 
    unread_token stream next_tok; 
    let subexp = parse_subexp () in
    parse_cond_subexps stream (subexp::acc)
  | _ -> raise @@ SyntaxError ("Unexpected token in cond expression: " ^ next_tok)

and parse_list_literal stream : c_exp list =
  let rec parse_next acc =
    eat_whitespaces stream;
    let ch = read_char stream in
    match ch with
    | ']' -> List.rev acc
    | ',' -> 
      let exp = parse_infix_exp stream in
      parse_next (exp::acc)
    | _ -> raise @@ SyntaxError ("Unexpected token in list literal: " ^ string_of_char ch)
  in
  eat_whitespaces stream;
  let first_exp = parse_infix_exp stream in
  parse_next [first_exp]

and parse_exp stream : c_exp =
  eat_whitespaces stream;
  let ch = read_char stream in

  if ch = '(' then
    let iexp = parse_infix_exp stream in
    check_closing_delim stream ')';
    iexp
  else

  if ch = '#' then
    let tok = read_token stream false in
    match tok with
    | "True"  -> Literal (Boolean true)
    | "False" -> Literal (Boolean false)
    | _ -> raise (SyntaxError "Error while parsing boolean literal")
  else 

  if is_digit ch then begin
    unread_char stream ch;
    let tok = read_token stream false in
    Literal (Fixnum (int_of_string tok))
  end else

  if ch = '[' then
    let ch' = read_char stream in
    match ch' with
    | ']' -> Literal Nil
    | _ -> unread_char stream ch'; ListLiteral (parse_list_literal stream)
  else 

  let _   = unread_char stream ch   in
  let tok = read_token stream false in

  if tok = "if" then
    let bexp = parse_infix_exp stream in
    check_next_keyword stream "then";
    let branch1 = parse_infix_exp stream in
    check_next_keyword stream "else";
    let branch2 = parse_infix_exp stream in
    If(bexp, branch1, branch2)
  else

  if tok = "let" then
    let name = read_token stream false in
    check_is_valid_ident name;
    check_next_keyword stream "=";
    let val_exp = parse_infix_exp stream in
    check_next_keyword stream "in";
    let body_exp = parse_infix_exp stream in
    Let (name, val_exp, body_exp)
  else

  if tok = "cond" then
    let subexps = parse_cond_subexps stream [] in
    Cond subexps
  else

  if is_valid_ident tok then begin
    eat_whitespaces stream;
    let ch = read_char stream in
    match ch with
    | '(' -> let args = parse_call_args stream [] in 
      begin
      eat_whitespaces stream;
      match read_char stream with
      | ')' -> Call (Ident tok, args)
      | _ -> raise (SyntaxError "Incorrect function call syntax")
      end
    | _ -> unread_char stream ch; Ident tok
  end else

  raise @@ SyntaxError ("Expression is not supported, current token = " ^ tok)

and parse_infix_exp stream =
  eat_whitespaces stream;
  let infix_op = ["*"; "+"; "-"; "/"; ">"; "<"; "=="; ":"; "&&"; "||"; "%"] in
  let is_infix_op c = List.mem c infix_op in

  let exp1 = parse_exp stream in
  
  let tok = read_token stream false in
  if is_infix_op tok then
    let exp2 = parse_exp stream in
    Call (Ident tok, [exp1; exp2])
  else begin
    unread_token stream tok; 
    exp1
  end

let parse_hl_exp stream =
  let parse_val_def () =
    let name = read_token stream false in
    check_is_valid_ident name;
    check_next_keyword stream "=";
    let val_exp = parse_infix_exp stream in
    DefVal (name, val_exp)
  in
  let parse_func_def () =
    let rec parse_args acc =
      match read_token stream false with
      | "nothing" -> if List.is_empty acc
        then begin
          check_next_keyword stream "=";
          acc
        end else raise (SyntaxError "Unit literal is in inappropriate place")
      | "=" -> acc
      | ident -> check_is_valid_ident ident; parse_args (acc @ [ident])
    in
    let fname = read_token stream false in
    check_is_valid_ident fname;
    let args = parse_args [] in
    let body = parse_infix_exp stream in
    DefFn (fname, args, body)
  in

  let tok = read_token stream false in
  let hl_exp = match tok with
  | "val" -> parse_val_def ()
  | "fn"  -> parse_func_def ()
  | _ -> unread_token stream tok; 
         HLExp (parse_infix_exp stream)
  in
  
  let maybe_hl_delim = read_token stream true in
  match maybe_hl_delim with
  | ";;" -> hl_exp
  | _ -> raise (SyntaxError ("Error at the end of highlevel expression, tok = " ^ maybe_hl_delim))

let rec parse_program stream : program =
  eat_whitespaces stream;
  if eof_in_stream stream 
    then []
    else let hl_exp = parse_hl_exp stream in
      hl_exp::(parse_program stream)

(* === High level === *)

let read_src_from_file filename =
  Core.In_channel.read_all filename

let parse_from_string src : program =
  let sr = constr_string_reader src in
  let stream = new_input_stream sr in
  parse_program stream

let parse_from_file filename : program =
  let src = read_src_from_file filename in
  parse_from_string src