%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE CMP EQ EOF
%token <int> INTLIT VARIABLE
%token <string> STRLIT

%left PLUS MINUS
%left TIMES DIVIDE
%left EQ

%start expr
%type < Ast.expr> expr

%%

expr:
| expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| expr CMP expr    { Binop($1, Cmp, $3) }
| VARIABLE EQ expr { Asn($1, $3) }
| INTLIT           { Lit($1) }
| STRLIT           { StrLit($1) }
| VARIABLE	   { Lit($1) }
