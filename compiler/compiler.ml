open Common
open Codegen

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

let generate_entrypoint' out_file : unit =
  let entrypoint = generate_globl_func "ultra_entrypoint" body in
  generate_prologue out_file;
  Printf.fprintf out_file "%s\n" entrypoint;
  generate_epilogue out_file

let generate_entrypoint out_file =
  generate_entrypoint' out_file

let compiler_entry () =
  (* ultra -> asm *)
  let out = open_out "runtime/ultra.s" in
  generate_entrypoint out;

  (* asm -> binary *)
  let _ = Sys.command "gcc -O3 -c runtime/*.s" in
  ();;