exception SyntaxError   of string
exception SemaError     of string
exception InternalError of string

let syntax_error (msg: string) = SyntaxError msg |> raise

let sema_error (msg: string) = SemaError msg |> raise

let should_not_reach_here (msg: string): 'a = InternalError msg |> raise
