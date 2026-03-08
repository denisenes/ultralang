open Shared
open Shared.Ir
open Shared.Log
open Shared.Isa

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
    | "word"  -> word_size
    | "hword" -> hword_size
    | s -> Printf.sprintf "Unknown range: '%s'" s |> Parser.error


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
  let as_operand (w: Instruction.width) (s: string): Instruction.operand =
  match int_of_string_opt s with
  | Some num -> begin match w with
    | W8  -> W8  num
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
  in

  let read_opnd (opcode: Instruction.opcode) (n: int): Instruction.operand =
    as_operand (Instruction.width opcode n) (Stream.read_token stream)
  in

  match Stream.peek_char stream with
  | c when c = sym_marker -> 
    let res = Meta.Label (read_symbol stream) |> Block.meta in
    Parser.consume_char stream ':';
    res
  | _ -> 
    begin
      let opcode = Stream.read_token stream |> Instruction.from_string in
      let instr = match Instruction.arity opcode with
      | 0 -> Instruction.constr0 opcode
      | 1 -> Instruction.constr1 opcode (read_opnd opcode 0) 
      | 2 -> Instruction.constr2 opcode (read_opnd opcode 0) (read_opnd opcode 1)
      | 3 -> Instruction.constr3 opcode (read_opnd opcode 0) (read_opnd opcode 1) (read_opnd opcode 2)
      | n -> Format.sprintf "Impossible number of operands: %d" n
          |> Shared.Utils.shouldNotReachHere
      in Block.instr instr
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
      let size = Stream.read_token stream |> as_range in
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
    LLIR (parse_blocks stream)
  | _ -> Shared.Ir.invalid_ir_kind "Channel" "LLIR"