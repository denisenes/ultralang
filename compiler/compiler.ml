open Shared.Common

let body = "\tmov eax, 42\n\tret\n"

let generate_globl_func name body =
    ".globl " ^ name ^ "\n" ^
    "\t.type " ^ name ^ ", @function\n" ^
    name ^ ":\n" ^
    body

let generate_prologue out_file : unit =
  Printf.fprintf out_file "%s"
(".intel_syntax noprefix\n\n" ^
".text\n" ^
"\t.p2align 4,,15\n")

let generate_epilogue _ : unit = ()

let generate_entrypoint' out_file : unit =
  let entrypoint = generate_globl_func "ultra_entrypoint" body in
  generate_prologue out_file;
  Printf.fprintf out_file "%s\n" entrypoint;
  generate_epilogue out_file

let generate_entrypoint out_file =
  generate_entrypoint' out_file

let compiler_entry () =
  (* ultra -> asm *)
  let out = open_out "ultra.s" in
  generate_entrypoint out;

  (* asm -> binary *)
  let res_code = Sys.command "make compile" in
  if res_code != 0 then raise (CompilationError "gcc: compilation error")