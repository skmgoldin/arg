%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA 
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ RETURN IF ELSE FOR WHILE EOF
%token <int> INTLIT VARIABLE
%token <string> STRLIT ID

%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
| expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| expr SEQUENCE expr      { Seq($1, $3) }
| expr CMP expr    { Cmp($1, $3) }
| VARIABLE ASSIGN expr    { Asn($1, $3) }
| INTLIT           { IntLit($1) }
| STRLIT           { StrLit($1) }
| VARIABLE	   { IntLit($1) }
