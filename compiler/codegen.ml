open Shared.Common

open Common

let fixnum_tag = 0b00
let fixnum_shift = 2
let fixnum_to_immid num = (num lsl fixnum_shift) lor fixnum_tag

let bool_mask  = 0b1111111
let bool_tag   = 0b0011111
let bool_shift = 7
let bool_to_immid = function
  | true  -> (1 lsl bool_shift) lor bool_tag
  | false -> (0 lsl bool_shift) lor bool_tag

let nil_mask = 0b11111111
let nil_tag  = 0b00101111
let nil_to_immid = nil_tag

let gen_nil () =
  [Mov (Op_reg RAX, Op_immid nil_to_immid)]

let gen_fixnum num =
  let tagged_num = fixnum_to_immid num in
  [Mov (Op_reg RAX, Op_immid tagged_num)]

let gen_bool (b : bool) = 
  [Mov (Op_reg RAX, Op_immid (bool_to_immid b))]

let gen_expr = function
  | Literal (Fixnum num) -> gen_fixnum num
  | Literal (Boolean b)  -> gen_bool b
  | Literal (Nil) -> gen_nil ()
  | _ -> raise (CompilationError "Not implemented yet")

let gen_exprs (asts : exp list) =
  let res = List.map (fun ast -> gen_expr ast) asts in
  res @ [[Ret]]