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
  | /* Nothing */                           { [], [] }
  | code expr                          { $2 :: $1 }

expr:
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | STRLITERAL          { StrLiteral($1) }
