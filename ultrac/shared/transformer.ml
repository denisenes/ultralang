open Ir
open Isa

type stage_data =
  | Nothing
  | Module  of module_desc
  | LLIR    of CUnit.t
  | Image   of bytes
  | String  of string
  | Channel of in_channel


type ir_transformer = stage_data -> stage_data

exception InvalidIRKind of string

let invalid_ir_kind (expected_in: string) (expected_out: string) =
  raise @@ InvalidIRKind (Format.sprintf "Expected: %s -> %s" expected_in expected_out)