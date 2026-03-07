open Shared
open Shared.Ir
open Shared.Log
open Shared.Isa

type width =
  | W8
  | W16

let sym_marker = '$'

let directives = [
  ".asm"; ".endasm";
  ".res"; ".function";
]

let is_directive (s: string) = List.mem s directives


let as_range (v: string): int =
  let open Shared.Utils in
  match int_of_string_opt v with
  | Some num -> num
  | None -> match v with
    | "word" -> Utils.word_size
    | "hword" -> Utils.hword_size
    | s -> Printf.sprintf "Unknown range: '%s'" s |> Parser.error


let as_operand (w: width) (s: string): Instruction.operand =
  match int_of_string_opt s with
  | Some num -> begin match w with
    | W8  -> W8 num
    | W16 -> W16 num
    end
  | None -> 
    begin if String.starts_with ~prefix:s "$" then
      Label (Utils.string_tail s 1)
    else
      (* Special values*)
      match s with
      | "eq" -> W8 0
      | "gt" -> W8 1
      | "lt" -> W8 (-1)
      | _ -> Printf.sprintf "Unknown operand: '%s'" s 
          |> Parser.error
    end
      


let read_symbol (stream: Stream.t): string =
  Parser.consume_char stream sym_marker;
  Parser.ident stream 


let consume_directive (stream: Stream.t) (expected: string): unit =
  let actual = Stream.read_token stream in
  if is_directive actual && actual = expected 
    then logMsg PARSER @@ Format.sprintf "Consumed directive %s" expected
    else Printf.sprintf "Unexpected token. Expected: '%s', actual: '%s'" expected actual 
      |> Parser.errorl stream


let parse_seq_elem stream: Block.elem =
  match Stream.peek_char stream with
  | sym_marker -> Meta (Label (read_symbol stream))
  | s -> 
    begin

    end


let rec parse_func_block stream (acc: Block.elem list): Block.elem list =
  let next = Stream.peak_token stream in
  if is_directive next then
    List.rev acc
  else
    parse_func_block stream (parse_seq_elem stream :: acc)


let parse_block stream: Block.t =
  let open Shared.Isa.Block in
  match Stream.read_token stream with
  | ".function" ->
    begin
      let iseq = parse_func_block stream [] in
      {
        kind       = Function;
        start_addr = None;
        size       = None;
        seq        = iseq;
        bin        = None
      }
    end
  | ".res" -> 
    begin
      let sym  = read_symbol stream in
      let size = read_token stream |> as_range in
      {
        kind       = Reserved;
        start_addr = None;
        size       = Some size;
        seq        = [Meta (Label sym)];
        bin        = None
      }
    end
  | token -> Printf.sprintf "Unknown directive: '%s'" token 
          |> Parser.errorl stream


let parse_blocks stream: Block.t list =
  consume_directive stream ".asm";
  Parser.go_until_token stream (fun _ ->
    parse_block stream
  ) ".endasm"
  


(** Parsing entrypoint *)
let parse (input: stage_data): stage_data =
  match input with
  | Channel chan ->
    let stream = Stream.create chan in
    Module (parse_module stream)
  | _ -> raise (InvalidIRKind "Parser expects channel as input")