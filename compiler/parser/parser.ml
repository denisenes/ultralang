open Shared.Ir


let parse (_ : [>`Nothing]): [>`Ast of ir_ast] = `Ast (Node "hello")