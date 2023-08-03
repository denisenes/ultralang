open Shared.Parser
open Shared.Printer
open Shared.Common

open Common
open Serializer

let body = [
  Mov (Op_reg RAX, Op_immid 42); 
  Mov (Op_reg RAX, Op_immid 123);
  Ret
  ]

let generate_globl_func name body =
    ".p2align 4,,15\n" ^
    ".globl " ^ name ^ "\n" ^
    "\t.type " ^ name ^ ", @function\n" ^
    name ^ ":\n" ^
    gen_instr_seq body

let generate_prologue out_file : unit =
  Printf.fprintf out_file "%s"
(".intel_syntax noprefix\n\n" ^
".text\n")

let generate_epilogue out_file : unit = Printf.fprintf out_file "\n"

let compile (_ : exp list) out_file =
  (* AST -> instruction sequence *)

  let entrypoint = generate_globl_func "ultra_entrypoint" body in
  generate_prologue out_file;
  Printf.fprintf out_file "%s\n" entrypoint;
  generate_epilogue out_file

let compiler_entry () =

  (* Parse src *)
  let asts = parse_from_file "test/sample.lsp" in
  let _ = List.map 
    (fun ast -> let res = ast_to_string ast in print_endline res) 
    asts in

  (* ultra -> asm *)
  let out = open_out "runtime/ultra.s" in
  compile asts out;

  (* asm -> binary *)
  let _ = Sys.command "gcc -O3 -c runtime/*.s" in
  let _ = Sys.command "rm *.o" in
  ();;