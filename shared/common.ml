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
    | Apply       of c_exp * c_exp list
    | Call        of name  * c_exp list
    | Lambda      of name list * c_exp
    | Cond        of (c_exp * c_exp) list
    | ListLiteral of c_exp list
    | ShouldNotReachHere of int
;;

type ty_primitive = [
    | `TyBool
    | `TyInt
]

type ty =  [
    | ty_primitive
    | `TyList of ty           (* List(t) *) (* TODO generalize to record type *)
    | `TyFun  of ty list * ty (* (t_1, t_2, ..., t_n) -> t_res *)
    | `TyVar  of name
]

type ty_scheme = 
    | Scheme of name list * ty (* forall a_1 a_2 ... a_n . t *)

exception SyntaxError     of string;;
exception TypeCheckError  of string;;
exception PrintError      of string;;
exception EvaluationError of string;;
exception CompilationError of string;;

exception NotFoundInEnv     of string;;
exception ValueNotSpecified of string;;

exception InternalError of string;;

(* TODO merge all primitive-function-related stuff
 * into one place
 *)

let unar_spec_bindings =
    [
        "inc"; "dec"; "not"; "head"; "tail";
        "is_nil"; "is_int"; "is_bool"
    ]
  
let bin_spec_bindings =
    [
        "+" ; "-" ; "*" ; "/" ; "==" ; ">" 
    ]
  
let spec_bindings =
    unar_spec_bindings @ bin_spec_bindings

let spec_binding_ty = function
    | "inc" | "dec"   -> 
        Some (Scheme ([], (`TyFun ([`TyInt], `TyInt))))
    | "not"           -> 
        Some (Scheme ([], (`TyFun ([`TyBool], `TyBool))))
    | "head" | "tail" -> 
        let lst_tpe = `TyList (`TyVar "a") in
        Some (Scheme (["a"], (`TyFun ([lst_tpe], lst_tpe))))
    | "is_nil" | "is_int" | "is_bool" ->
        Some (Scheme (["a"], (`TyFun ([`TyVar "a"], `TyBool))))
    | "+" | "-" | "*" | "/" | "%" ->
        Some (Scheme ([], (`TyFun ([`TyInt; `TyInt], `TyInt))))
    | "==" | ">" | "<" ->
        Some (Scheme ([], (`TyFun ([`TyInt; `TyInt], `TyBool))))
    | "&&" | "||" ->
        Some (Scheme ([], (`TyFun ([`TyBool; `TyBool], `TyBool))))
    | _ -> None