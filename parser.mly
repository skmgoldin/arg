%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA 
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT GT LEQ GEQ
%token IF ELSE WHILE FOR RET
%token STOP EXC
%token EOF
%token <int> INTLITERAL
%token <string> STRLITERAL
%token <string> VAR

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
  | decls vdecl { ($2 :: fst $1), snd $1 }
  | decls fdecl { fst $1, ($2 :: snd $1) }

vdecl:
  | VAR SEMI { $1 }
  | VAR ASSIGN INTLITERAL { $1 }
  | VAR ASSIGN STRLITERAL { $1 }

vdecl_list:
  | /* Nothing */     { [] }
  | vdecl_list vdecl  { $2 :: $1 }

stmt_list:
  | /* Nothing */     { [] }
  | stmt_list stmt    { $2 :: $1 }

stmt:
  | expr SEMI         { expr($1) }
  | RET expr SEMI     { Ret($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev($2)) }

fdecl:
  VAR LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    { { fname   = $1;
        formals = $3;
        locals  = List.rev $6;
        body    = List.rev $7 } }

formals_opt:
  | /* Nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
  | VAR                    { [$1] }
  | formal_list COMMA VAR  { $3 :: $1 }

expr:
  | INTLITERAL                    { IntLiteral($1) }
  | STRLITERAL                    { StrLiteral($1) }
  | VAR                           { Var($1) }
  | expr PLUS expr                { Binop($1, Add, $3) }
  | expr MINUS expr               { Binop($1, Sub, $3) }
  | expr TIMES expr               { Binop($1, Mult, $3) }
  | expr DIVIDE expr              { Binop($1, Div, $3) }
  | expr EQ expr                  { Binop($1, Eq, $3) }
  | expr NEQ expr                 { Binop($1, Neq, $3) }
  | expr LT expr                  { Binop($1, Less, $3) }
  | expr LEQ expr                 { Binop($1, Leq, $3) }
  | expr GT expr                  { Binop($1, Greater, $3) }
  | expr GEQ expr                 { Binop($1, Geq, $3) }
  | VAR ASSIGN expr               { Assign($1, $3) }
  | VAR LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN            { $2 }
