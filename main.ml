let () =
  let input = Lexing.from_string "(\n nil)" in
  try
    let result = Parser.main Lexer.token input in
    Syntax.to_s result |> print_endline
  with
  | Lexer.Error msg ->
    Printf.printf "Lexer Error %s" msg
  | Parser.Error ->
    Printf.printf "Parser error"
      
      
