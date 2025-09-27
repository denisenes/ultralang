type ir_ast =
  Node of string


type ir_kind = [
  | `Nothing
  | `Ast of ir_ast 
  | `String of string
  | `Channel of in_channel
]