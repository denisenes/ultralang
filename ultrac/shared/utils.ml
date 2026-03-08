let word_size  = 2 (* bytes *)
let hword_size = 1 (* byte *)

exception InternalError of string

let shouldNotReachHere (msg: string): 'a = InternalError msg |> raise

let rec string_rep (s: string) (i: int): string =
  match i with
  | 0 -> ""
  | 1 -> s
  | _ -> String.cat s @@ string_rep s (i - 1)


let string_tail (s: string) (index: int): string =
  String.sub s index (String.length s - 1)