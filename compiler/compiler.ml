open Shared.Parser

open Serializer

let generate_prologue out_file : unit =
  Printf.fprintf out_file "%s"
  (".intel_syntax noprefix\n\n" ^
  ".text\n")

let generate_epilogue out_file : unit = 
  Printf.fprintf out_file "\n"

let serialize out_file =
  let serialized = serialize_all() in
  Printf.fprintf out_file "%s\n" serialized

let print_header =
  Printf.printf "Ultralang compiler x64\n\n"

let compile asts =
  print_header;

  Printf.printf "AST:\n";
  let _ = List.map (fun ast -> 
    let res = Shared.Printer.ast_to_string ast 0 in print_endline res
  ) asts
  in
  Printf.printf "\n\n================\n";

  (* AST -> instructions *)
  Codegen.gen_highelevel_exprs asts;

  Funcmap.log_func_map();
  Printf.printf "\n\n================\n";

  (* Emit instructions *)
  let out = open_out "runtime/ultra.s" in
  generate_prologue out;
  serialize out;
  generate_epilogue out;
  ();;

let compiler src_path =
  let asts = parse_from_file src_path in
  compile asts

let compiler_test src =
  let asts = parse_from_string src in
  compile asts