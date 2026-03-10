open Shared.Transformer

(** Computes blocks placement inside image and fills symbol table *)
let index: ir_transformer = fun input ->
  match input with
  | LLIR ir -> LLIR ir
  | _ -> invalid_ir_kind "LLIR" "LLIR"