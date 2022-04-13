open Printf

type ast =
  | Int of int
  | Bool of bool
  | Nil
  | Id of string
  | Sexp of ast list


let rec to_s =  function
  | Int i -> sprintf "%d" i
  | Bool b -> if b then "true" else "false"
  | Nil -> "nil"
  | Id x -> x
  | Sexp e -> "(" ^ List.fold_right (fun a b -> to_s a ^ " " ^ b) e "" ^ ")"
