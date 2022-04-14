[@@@warning "-33"]
open Syntax

module Result = struct
  type 'a t = Ok of 'a | Error of string

  let bind a f =
    match a with
    | Ok x -> f x
    | (Error _) as err -> err

  let return x = Ok x
end

module Ast = struct
  type typ =
    | TInt
    | TBool
    | TArrow of typ * typ
    | TVar of string
    | TUnk

  module Env = Map.Make(String)

  type t =
    | Int of int
    | Bool of bool
    | Nil
    | Var of string * typ
    | Lambda of string list * typ list * t
    | Closure of t * env
    | Appl of t list
  and env = t Env.t

  let rec typ_of_sexp : Syntax.sexp -> typ = fun _ -> TUnk
  and typ_of_sexps sexps =
    List.map typ_of_sexp sexps

  let ids (sexps : sexp list) : string list =
    List.map (function
        | Id x -> x
        | _ -> failwith  "Expecting an identifier")
      sexps

  let rec of_sexp : Syntax.sexp -> t = function
    | Syntax.Int i -> Int i
    | Syntax.Bool b -> Bool b
    | Syntax.Nil -> Nil
    | Id i -> Var (i, TUnk)
    (* (lambda (x) (int int)  x) *)
    | Sexp [                
        Id "lambda";        
        Sexp args;
        Sexp typs;
        body] -> Lambda (ids args,  typ_of_sexps typs, of_sexp body)
    | Sexp exprs ->
      Appl (of_sexps exprs)
  and of_sexps sexps =
    List.map of_sexp sexps

  let rec eval (expr : t) (env : env) : t =
    match expr with
    | Appl (op :: args) -> apply op args env
    | Appl [] -> failwith "empty application"
    | Var (x, _) -> Env.find x env
    | Lambda _ -> Closure (expr, env)
    | _ -> expr
  and apply (expr : t) (args : t list) (env : env) : t =
    match expr with
    | Lambda (parms, _, body) ->
      if List.length args <> List.length parms
      then failwith "Wrong number of arguments"
      else
        let pairs = List.combine parms args |> List.to_seq in
        let env = Env.add_seq pairs env in
        eval body env
    | _ -> failwith "apply exploded"


  let rec to_str (expr : t) : string =
    let p = Printf.sprintf in
    let concat_str = List.fold_left (p "%s %s") " " in
    match expr with
    | Closure (Lambda (args, _, body), _) | Lambda (args, _, body)
      ->
      let args = concat_str args in
      let body = to_str body in 
      p "(lambda (%s) (_) %s)" args body
    | Closure _ -> failwith "bad closure"
    | Var (x, _) -> x
    | Appl args -> concat_str (List.map to_str args)
    | Int x -> string_of_int x
    | _ -> failwith "can't convert to string"


end

let parse ?(ch = stdin) () : sexp =
  let input = Lexing.from_channel ch in
  try
    Parser.main Lexer.token input
  with
  | Lexer.Error msg ->
    failwith @@ Printf.sprintf "Lexer Error %s" msg
  | Parser.Error ->
    failwith "Parser error"

let main () =
 let open Result in
 let sexp = parse () in
 let ast = Ast.of_sexp sexp in
 let env = Ast.Env.empty in
 let res = Ast.eval ast env in
 res |> Ast.to_str |> print_endline

let () =
  main () |> ignore



