%{ open Ast %}

%token PRINT
%token COMMA SEMI
%token LPAREN RPAREN DQUOTE
%token ASSIGN
%token EOF
%token <string> STRLITERAL 
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
