open Shared.Parser2

open Serializer

let generate_externs () =
  List.fold_left (fun acc f ->
    ".extern " ^ f ^ "\n" ^ acc
  ) "" Common.runtime_bindings

let generate_prologue out_file : unit =
  Printf.fprintf out_file "%s"
  (".intel_syntax noprefix\n\n" ^
  generate_externs()            ^
  "\n.text\n")

let generate_epilogue out_file : unit = 
  Printf.fprintf out_file "\n"

let serialize out_file =
  let serialized = serialize_all() in
  Printf.fprintf out_file "%s\n" serialized

let print_header() =
  Printf.printf "ULTRA compiler x64\n\n"

let translate asts =
  (* AST -> instructions *)
  Context.init_context();
  Codegen.gen_highelevel_exprs asts;

  Context.log_func_map();
  Printf.printf "\n\n================\n";

  (* Emit instructions *)
  let out = open_out "runtime/ultra.s" in
  generate_prologue out;
  serialize out;
  generate_epilogue out;
  ();;

let compiler src_path =
  print_header();

  let asts = parse_from_file src_path in
  
  Printf.printf "ASTs:\n";
  Shared.Printer.print_asts asts;
  Printf.printf "\n\n================\n";

  let lowered_asts = List.map
    (fun ast -> Shared.Asttransform.lower_hl_entry ast) asts
  in

  Printf.printf "Lowered ASTs:\n";
  Shared.Printer.print_asts lowered_asts;
  Printf.printf "\n\n================\n";

  translate lowered_asts

let compiler_test src =
  let asts = parse_from_string src in
  translate asts