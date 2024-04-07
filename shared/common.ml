type 'a env = (string * 'a option ref) list

type name  = string

type value = 
    Fixnum    of int                                |
    Boolean   of bool                               |
    Symbol    of string                             |
    Pair      of value * value                      |
    Primitive of string * (value list -> value)     |
    Closure   of name list * c_exp * value env      |
    Quote     of value                              | 
    Nil

and program = hl_entry list

and hl_entry =
    | DefVal of name * c_exp
    | DefFn  of name * name list * c_exp
    | HLExp  of c_exp

and c_exp   =
    | Literal     of value
    | Ident       of name
    | If          of c_exp * c_exp * c_exp
    | Let         of name * c_exp * c_exp
    | Apply       of c_exp * (c_exp list)
    | Call        of c_exp * c_exp list
    | Lambda      of name list * c_exp
    | Cond        of (c_exp * c_exp) list
    | ListLiteral of c_exp list
    | ShouldNotReachHere of int
;;

exception SyntaxError     of string;;
exception PrintError      of string;;
exception EvaluationError of string;;

exception NotFoundInEnv     of string;;
exception ValueNotSpecified of string;;

exception CompilationError of string;;

let unar_spec_bindings =
    [
        "inc"; "dec"; "is_zero"; "not"; "head"; "tail";
        "is_nil"; "is_int"; "is_bool"
    ]
  
let bin_spec_bindings =
    [
        "+" ; "-" ; "*" ; "/" ; "==" ; ">" 
    ]
  
let spec_bindings =
    unar_spec_bindings @ bin_spec_bindings