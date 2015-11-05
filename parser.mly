%{ open Ast %}

%token PRINT
%token LPAREN RPAREN SEMI
%token ASSIGN
%token EOF
%token <int> INTLITERAL
%token <string> VAR

%right ASSIGN

%start program
%type <Ast.program> program

%%

program:
  stmts EOF                       { $1 }

stmts:
  | /* Nothing */                 { [], [] }
  | stmts vdecl                   { ($2 :: fst $1), snd $1 }

vdecl:
  | VAR SEMI                      { $1 }

