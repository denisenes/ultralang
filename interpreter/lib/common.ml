type lobject = 
    Fixnum    of int                                |
    Boolean   of bool                               |
    Symbol    of string                             |
    Pair      of lobject * lobject                  |
    Primitive of string * (lobject list -> lobject) |
    Nil

exception SyntaxError     of string;;
exception PrintError      of string;;
exception NotFoundInEnv   of string;;
exception EvaluationError of string;;