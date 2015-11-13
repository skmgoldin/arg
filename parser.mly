%{ open Ast %}

%token COMMA SEMI
%token LPAREN RPAREN
%token ASSIGN
%token <string> STRLITERAL
%token <string> ID
%token EOF

%right ASSIGN

%start program
%type <Ast.program> program

%%

program:
  code EOF                       { $1 }

code:
  | /* Nothing */                { [] }
  | code expr                    { $2 :: $1 }

expr:
  | ID ASSIGN expr               { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN SEMI { Call($1, $3) }
  | STRLITERAL SEMI                  { StrLiteral($1) }

actuals_opt:
  | /* Nothing */                { [] }
  | actuals_list                 { List.rev $1 }

actuals_list:
  | expr                         { [$1] }
  | actuals_list COMMA expr      { $3 :: $1 }
