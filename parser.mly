%token <int> INT
%token EOF
%token TRUE
%token FALSE
%token NIL
%token LPAR
%token RPAR
%token <string> ID
%start <Syntax.ast> main
%{ open Syntax %}

%%

let main :=
~ = expr; EOF; <>

let expr :=
  | ~ = INT; <Int>
  | TRUE; { Bool true }
  | FALSE; { Bool false }
  | NIL; { Nil }
  | ~ = ID; <Id>
  | LPAR; ~ = nonempty_list(expr); RPAR; <Sexp>

