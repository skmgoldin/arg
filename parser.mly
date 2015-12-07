%{ open Ast %}

%token ADD SUB MULT DIV EQUAL NEQ LESS LEQ GREATER GEQ
%token COMMA SEMI
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token ASSIGN
%token FUNCTION WHILE IF ELSE PRINT
%token <string> STRLITERAL
%token <int>    INTLITERAL
%token <float>  FLOATLITERAL
%token <bool>   BOOLLITERAL
%token <string> ID
%token EOF

%right ASSIGN
%left EQUAL NEQ
%left LESS GREATER LEQ GEQ
%left ADD SUB
%left MULT DIV

%start program
%type <Ast.program> program

%%

program:
  code EOF                             { $1 }

code:
  | functions body                     { ($1, $2) }

body:
  | /* nothing */                      { [] }
  | body statement                     { List.rev ($2 :: $1) }

functions:
  | /* nothing */                     { [] }
  | functions func                    { List.rev ($2 :: $1) }

func:
  | FUNCTION ID LPAREN params_opt RPAREN LBRACE stmt_opt RBRACE
    {
      { fname = $2;
        formals = $4;
        body = $7; }
    }

statement:
  | expr SEMI                         { Expr($1) }
  | IF LPAREN expr RPAREN LBRACE
    stmt_opt RBRACE ELSE LBRACE
    stmt_opt RBRACE                   { IfElse($3, $6, $10) }
  | IF LPAREN expr RPAREN LBRACE
    stmt_opt RBRACE                   { If($3, $6) }
  | WHILE LPAREN expr RPAREN LBRACE stmt_opt RBRACE { While($3, $6) }
  | ID LBRACK INTLITERAL RBRACK ASSIGN LBRACE actuals_opt RBRACE SEMI     { ArrayAssign($1, $3, $7) }
  | PRINT LPAREN STRLITERAL COMMA expr RPAREN SEMI { Print($3, $5) }

expr:
  | ID ASSIGN expr                     { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN       { Call($1, $3) }
  | STRLITERAL                         { StrLiteral($1) }
  | INTLITERAL                         { IntLiteral($1) }
  | FLOATLITERAL                       { FloatLiteral($1) }
  | BOOLLITERAL                        { BoolLiteral($1) }
  | ID                                 { Id($1) }
  | expr ADD expr                      { Binop($1, Add, $3) }
  | expr SUB expr                      { Binop($1, Sub, $3) }
  | expr MULT expr                     { Binop($1, Mult, $3) }
  | expr DIV expr                      { Binop($1, Div, $3) }
  | expr EQUAL expr                    { Binop($1, Equal, $3) }
  | expr NEQ expr                      { Binop($1, Neq, $3) }
  | expr LESS expr                     { Binop($1, Less, $3) }
  | expr LEQ expr                      { Binop($1, Leq, $3) }
  | expr GREATER expr                  { Binop($1, Greater, $3) }
  | expr GEQ expr                      { Binop($1, Geq, $3) }
  | LPAREN expr RPAREN                 { $2 }

stmt_opt:
  | /* Nothing */                      { [] }
  | stmt_list                          { $1 }

stmt_list:
  | statement                          { [$1] }
  | stmt_list statement                { List.rev ($2 :: $1) }

actuals_opt:
  | /* Nothing */                      { [] }
  | actuals_list                       { $1 }

actuals_list:
  | expr                               { [$1] }
  | actuals_list COMMA expr            { List.rev ($3 :: $1) }

params_opt:
  | /* nothing */                     { [] }
  | params_list                       { $1 }

params_list:
  | ID                                { [$1] }
  | params_list COMMA ID              { List.rev ($3 :: $1) }
