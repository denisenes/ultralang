open Shared.Parser
open Shared.Printer

open Common
open Codegen
open Serializer

let body = [
  Mov (Op_reg RAX, Op_immid 42); 
  Mov (Op_reg RAX, Op_immid 123);
  Ret
  ]

let generate_globl_func name body =
    ".global " ^ name ^ "\n" ^
    "\t.type " ^ name ^ ", @function\n" ^
    name ^ ":\n" ^ body

let generate_prologue out_file : unit =
  Printf.fprintf out_file "%s"
(".intel_syntax noprefix\n\n" ^
".text\n")

let generate_epilogue out_file : unit = Printf.fprintf out_file "\n"

let serialize (instr_seqs : instruction list list) out_file =
  let body = List.fold_left
    (fun b seq -> b ^ serialize_instr_seq seq) ""  instr_seqs in
  let entrypoint = generate_globl_func "ultra_entrypoint" body in
  Printf.fprintf out_file "%s\n" entrypoint

let compiler () =
  (* Parse src *)
  let asts = parse_from_file "test/mul.lsp" in
  let _ = List.map 
    (fun ast -> let res = ast_to_string ast in print_endline res) 
    asts in

  (* AST -> instructions *)
  let instr_seqs = gen_exprs asts in

  (* Emit instructions *)
  let out = open_out "runtime/ultra.s" in
  generate_prologue out;
  serialize instr_seqs out;
  generate_epilogue out;
  ();;