type 'a env = (string * 'a option ref) list

type lobject = 
    Fixnum    of int                                |
    Boolean   of bool                               |
    Symbol    of string                             |
    Pair      of lobject * lobject                  |
    Primitive of string * (lobject list -> lobject) |
    Closure   of name list * exp * value env        |
    Quote     of value                              | 
    Nil

and value = lobject
and name  = string
and exp   =
    | Literal of value
    | Var of name
    | If of exp * exp * exp
    | And of exp * exp
    | Or  of exp * exp
    | Apply of exp * exp
    | Call of exp * exp list
    | Lambda of name list * exp
    | Defexp of def

and def = 
    | Val   of name * exp
    | FnDef of name * name list * exp
    | Exp   of exp

exception SyntaxError     of string;;
exception PrintError      of string;;
exception EvaluationError of string;;

exception NotFoundInEnv     of string;;
exception ValueNotSpecified of string;;