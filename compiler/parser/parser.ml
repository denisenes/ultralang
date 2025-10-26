open Shared.Ir
open Shared.Log
open Stream

exception SyntaxError of string

let error (msg: string) = 
  raise @@ SyntaxError msg

let errorl (stream: Stream.t) (msg: string) = 
  let prefix = Stream.current_pos stream in
  error (prefix ^ " " ^ msg)


let is_valid_ident ?(kind: Token.identifier_kind = Lower) (ident: string): bool =
  if Token.is_keyword ident 
    then false
    else
      let first_char = String.get ident 0 in
      match kind with
      | Lower -> 
        (Token.is_lower_identifier_start first_char) &&
        String.for_all Token.is_lower_identifier_char ident
      | Upper ->
        (Token.is_upper_identifier_start first_char) &&
        String.for_all Token.is_upper_identifier_char ident      


let ident ?(kind: Token.identifier_kind = Lower) (stream: Stream.t): string =
  let ident = read_token stream in
  if is_valid_ident ~kind ident
    then ident
    else error @@ Printf.sprintf "Incorrect identifier: '%s'" ident


let consume_keyword (stream: Stream.t) (expected: string): unit =
  let actual = read_token stream in
  if Token.is_keyword actual && actual = expected 
    then begin
      logMsg PARSER @@ Format.sprintf "Consumed keyword %s" expected;
      ()
    end else errorl stream @@ Printf.sprintf "Unexpected token. Expected: '%s', actual: '%s'" expected actual


let consume_char (stream: Stream.t) (expected: char) =
  skip_whitespaces stream;
  let c = read_char stream in
  if c = expected
    then ()
    else errorl stream @@ Printf.sprintf "Unexpected character: '%c'" c


let go_until (stream: Stream.t) (action: (Stream.t -> 'a)) (pred: unit -> bool): 'a list =
  let rec consume_until' acc: 'a list =
    if pred()
      then acc 
      else consume_until' (acc @ [action stream])  
  in
  consume_until' []


let go_until_token (stream: Stream.t) (action: (Stream.t -> 'a)) (expected: string) =
  let res = go_until stream action (fun _ ->
      peak_token stream |> (fun x -> x = expected)) 
  in
  let _ = consume_keyword stream expected in
  res


let go_until_char (stream: Stream.t) (action: (Stream.t -> 'a)) (expected: char) =
  let res = go_until stream action (fun _ -> 
    skip_whitespaces stream; 
    peek_char stream |> (fun x -> x = expected)
  )
  in
  let _ = consume_char stream expected in
  res


let choice (stream: Stream.t) (variants: (string * (Stream.t -> 'a)) list): 'a =
  let tok = read_token stream in
  match List.find_opt (fun (x, _) -> x = tok) variants with
  | None -> errorl stream (Printf.sprintf "Unexpected token %s" tok)
  | Some (_, action) -> action stream


let if_next_char (stream: Stream.t) (expected: char) (action: Stream.t -> 'a) (otherwise: Stream.t -> 'a) =
  skip_whitespaces stream;
  let ch = peek_char stream in
  if ch = expected then
    let _ = consume_char stream expected in
    action stream
  else
    otherwise stream


let rec parse_type_app stream : utype list =
  let fst = parse_type_expr stream in
  let tail = go_until_char stream (fun s -> consume_char s ','; parse_type_expr s) ']' in
  fst :: tail


and parse_type_expr (stream: Stream.t): utype =
  skip_whitespaces stream;

  if_next_char stream '$' (fun _ -> 
    Var (ident ~kind:Upper stream)
  ) ( fun _ ->
    let tname = ident ~kind:Upper stream in
    if_next_char stream '[' (fun _ -> 
      let args = parse_type_app stream in
      App (tname, args)
    ) (fun _ -> 
      Atom tname
    )
  )


let parse_def_args stream: string list =
  consume_char stream '(';
  match read_char stream with
  | ')' -> []
  | c   ->
    unread_char stream c;
    let fst = ident stream in
    let tail = go_until_char stream (fun s -> consume_char s ','; ident s) ')' in
    fst :: tail


let rec parse_call_args stream: Node.t list =
  consume_char stream '(';
  match read_char stream with
  | ')' -> []
  | c   ->
    unread_char stream c;
    let fst = parse_infix_exp stream in
    let tail = go_until_char stream (fun s -> consume_char s ','; parse_infix_exp s) ')' in
    fst :: tail


and parse_list_literal stream : Node.t list =
  match read_char stream with
  | ']' -> error "Internal error" (* This case should be checked by caller *)
  | c   ->
    unread_char stream c;
    let fst = parse_infix_exp stream in
    let tail = go_until_char stream (fun s -> consume_char s ','; parse_infix_exp s) ']' in
    fst :: tail


and parse_cond_subexps stream: (Node.t * Node.t) list =
  go_until_token stream (fun _ ->
    consume_keyword stream "|";
    let condition = parse_infix_exp stream in
    consume_keyword stream "=>";
    let branch = parse_infix_exp stream in
    (condition, branch)
  ) "endcond"


and parse_exp (stream: Stream.t) : Node.t =
  let open Shared.Ir.Node in

  skip_whitespaces stream;
  let line = stream.line_num in

  let ch = read_char stream in

  if ch = '(' then
    let iexp = parse_infix_exp stream in
    consume_char stream ')';
    iexp
  else

  if ch = '#' then (* TODO remove and represent as ADT *)
    match read_token stream with
    | "True"  -> literal (Bool true) ~line:line ()
    | "False" -> literal (Bool false) ~line:line ()
    | _ -> errorl stream "Error while parsing boolean literal"
  else 

  if Token.is_digit ch || ch = '-' then begin
    unread_char stream ch;
    literal (Int (stream |> read_token |> int_of_string)) ~line:line ()
  end else

  if ch = '[' then
    let ch' = read_char stream in
    match ch' with
    | ']' -> literal Nil ~line:line ()
    | _   -> 
      unread_char stream ch'; 
      let args = parse_list_literal stream in
      node ListLiteral ~args:args ~line:line ()
  else 

  let _   = unread_char stream ch   in
  let tok = read_token stream in

  if tok = "if" then
    let condition = parse_infix_exp stream in
    consume_keyword stream "then";
    let branch1 = parse_infix_exp stream in
    consume_keyword stream "else";
    let branch2 = parse_infix_exp stream in
    node If ~args:[condition; branch1; branch2] ~line:line ()
  else

  if tok = "let" then
    let binding = identifier (ident stream) () in
    consume_keyword stream "=";
    let val_exp = parse_infix_exp stream in
    consume_keyword stream "in";
    let body_exp = parse_infix_exp stream in
    node Let ~args:[binding; val_exp; body_exp] ~line:line ()
  else

  if tok = "cond" then
    let args = parse_cond_subexps stream in
    (* Cond node args are stored as:
       [c1; b1; c2; b2 ... cn; bn]
    *)
    node Cond 
      ~args:(List.concat_map (fun (c, b) -> [c; b]) args)
      ~line:line ()
  else

  if tok = "fn" then
    let args = parse_def_args stream in
    consume_keyword stream "=";
    let body = parse_infix_exp stream in
    node Lambda 
      ~args:((List.map (fun a -> identifier a ()) args) @ [body])
      ~line:line ()
  else

  if is_valid_ident tok then begin
    skip_whitespaces stream;
    let ch = read_char stream in
    match ch with
    | '(' ->
      let _ = unread_char stream ch in
      let args = parse_call_args stream in 
      node Call ~name:(Some tok) ~args:args ~line:line ()
    | _ -> unread_char stream ch; identifier tok ()
  end else

  errorl stream (Printf.sprintf "Unexpected token: %s" tok)


and parse_infix_exp (stream: Stream.t): Node.t =
  let open Shared.Ir.Node in

  skip_whitespaces stream;
  let line = stream.line_num in

  let is_infix_op token = 
    token <> "" 
    && not (Token.is_keyword token) 
    && String.for_all (fun c -> List.mem c Token.infix_op_char) token 
  in

  let exp1 = parse_exp stream in
  let token = read_token stream in
  if is_infix_op token then
    let exp2 = parse_exp stream in
    node Call ~name:(Some token) ~args:[exp1; exp2] ~line:line ()
  else begin
    unread_token stream token;
    exp1
  end


let parse_def (stream: Stream.t): def_desc =
  skip_whitespaces stream;
  let line = stream.line_num in
  choice stream [
    ("const", fun stream ->
      let name = ident stream in
      Stream.with_def stream (Some name) (fun () ->
        consume_keyword stream "=";
        let body = parse_infix_exp stream in
        {kind=Const; name=name; args=[]; line=line; def_type=None; body_tree=Some body}
      )
    );
    ("type", fun stream ->      
      let (name, params) = parse_type_constr stream in
      consume_keyword stream "";
      let variants = parse_data_constrs stream in
      
    );
    ("fn", fun stream ->
      let name = ident stream in
      Stream.with_def stream (Some name) (fun () ->
        let args = parse_def_args stream in
        let tpe = if_next_char stream ':' (fun _ ->
          Some (parse_type_expr stream)
        ) (fun _ -> None)
        in
        consume_keyword stream "=";
        let val_exp = parse_infix_exp stream in
        {kind=FuncDef; name=name; args=args; line=line; def_type=tpe; body_tree=Some val_exp}
      )
    );
    ("decl", fun stream -> (* as FuncDef but without body *)
      let name = ident stream in
      let args = parse_def_args stream in
      consume_char stream ':';
      let tpe = parse_type_expr stream in
      {kind=FuncDecl; name=name; args=args; line=line; def_type=(Some tpe); body_tree=None}
    );
  ]


let parse_module stream: module_desc =
  logScoped PARSER "parsing module" (fun () ->
    try
      consume_keyword stream "module";
      let name = ident ~kind:Token.Upper stream in
      Stream.with_module stream (Some name) (fun () -> 
        consume_keyword stream "begin";

        let defs = go_until_token stream parse_def "end" in

        {
          name=name; 
          imports=[]; (* TODO parse *)
          exports=[]; (* TODO parse *)
          defs=defs
        }
      )
    with (e: exn) -> 
        print_endline (Printexc.to_string e);
        exit 1
  )


(** Parsing entrypoint *)
let parse (input: stage_data): stage_data =
  match input with
  | Channel chan ->
    let stream = Stream.create chan in
    Module (parse_module stream)
  | _ -> raise (InvalidIRKind "Parser expects channel as input")