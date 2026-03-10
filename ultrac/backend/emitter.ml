open Shared.Transformer

(** Create ucorn image by LLIR *)
let emit_image: ir_transformer = fun input ->
  match input with
  | LLIR _ -> Image Bytes.empty
  | _ -> invalid_ir_kind "LLIR" "Image"