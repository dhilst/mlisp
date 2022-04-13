{
open Parser
exception Error of string
}


rule token = parse
  | ['0'-'9']+ as i { INT (int_of_string i) }
  | "true" { TRUE }
  | "false" { FALSE }
  | "nil" { NIL }
  | "()" { NIL }
  | '(' { LPAR }
  | ')' { RPAR }
  | [' ' '\t' '\n'] { token lexbuf }
  | [^ ' ' '\t' '(' ')' '\n']+ as i { ID i } 
  | eof { EOF } 
  | _ { raise (Error "Unknown token") }
    

