open Shared.Parser

open Common

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
    (fun b seq -> b ^ Serializer.serialize_instr_seq seq) ""  instr_seqs in
  let entrypoint = generate_globl_func "ultra_entrypoint" body in
  Printf.fprintf out_file "%s\n" entrypoint

let compile asts =
  let _ = List.map 
    (fun ast -> let res = Shared.Printer.ast_to_string ast in print_endline res) 
    asts in

  (* AST -> instructions *)
  Stack.create_new_frame();
  let instr_seqs = Codegen.gen_exprs asts in

  (* Emit instructions *)
  let out = open_out "runtime/ultra.s" in
  generate_prologue out;
  serialize instr_seqs out;
  generate_epilogue out;
  ();;

let compiler src_path =
  let asts = parse_from_file src_path in
  compile asts

let compiler_test src =
  let asts = parse_from_string src in
  compile asts