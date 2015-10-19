%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EQ EOF
%token <int> LITERAL VARIABLE

%left PLUS MINUS
%left TIMES DIVIDE
%left EQ

%start expr
%type < Ast.expr> expr

%%

expr:
  expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| VARIABLE EQ expr { Asn($1, $3) }
| LITERAL          { Lit($1) }
| VARIABLE	   { Lit($1) }
