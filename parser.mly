%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA 
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT GT LEQ GEQ
%token IF ELSE WHILE FOR RET
%token STOP EXC
%token EOF
%token <int> LITERAL
%token <string> VARIABLE

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

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
