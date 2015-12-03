%{ open Ast %}

%token COMMA SEMI
%token LPAREN RPAREN
%token LBRACE RBRACE
%token ASSIGN
%token FUNCTION WHILE
%token <string> STRLITERAL
%token <string> ID
%token EOF

%right ASSIGN

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
  | FUNCTION ID LPAREN params_opt RPAREN LBRACE func_body RBRACE
    {
      { fname = $2;
        formals = $4;
        body = $7; } 
    }

func_body:
  | /* nothing */                      { [] }
  | body statement                     { List.rev ($2 :: $1) }

statement:
  | expr SEMI                          { Expr($1) }
  | WHILE LPAREN expr RPAREN statement { While($3, $5) }

expr:
  | ID ASSIGN expr                     { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN       { Call($1, $3) }
  | STRLITERAL                         { StrLiteral($1) }
  | ID                                 { Id($1) }

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
