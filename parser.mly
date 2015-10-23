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

program:
  decls EOF { $1 }

decls:
  /* Nothing */ { [], [] }
