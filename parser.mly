%{ open Ast %}

%token COMMA SEMI DQUOTE
%token ASSIGN
%token EOF
%token <string> STRLITERAL 
%token <string> ID

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
  | ID ASSIGN expr SEMI          { $1 }

expr:
  | STRLITERAL                    { $1 }
  | ID                           { $1 }
