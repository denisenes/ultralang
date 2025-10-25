let rec string_rep (s: string) (i: int): string =
  match i with
  | 0 -> ""
  | 1 -> s
  | _ -> String.cat s @@ string_rep s (i - 1) 