%{ open Ast %}

%token LBRACE RBRACE SEMI COMMA 
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT GT LEQ GEQ
%token CINT CCHAR CDOUBLE CBOOL SOF TOF EXC
%token LBRAC RBRAC
%token IF ELSE WHILE RET STOP FUNC
%token EOF
%token INT BOOLEAN DOUBLE CHAR
%token <int> INTLITERAL
%token <float> FLOATLITERAL
%token <string> STRLITERAL
%token <bool> BOOLLITERAL
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
  decls EOF                       { $1 }

decls:
  /* Nothing */                   { [], [] }
  | decls vdecl                   { ($2 :: fst $1), snd $1 }
  | decls fdecl                   { fst $1, ($2 :: snd $1) }

fdecl:
  VAR LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
                                  { { fname   = $1;
                                      formals = $3;
                                      locals  = List.rev $6;
                                      body    = List.rev $7 } }

formals_opt:
   /* Nothing */                 { [] }
  | formal_list                   { List.rev $1 }

formal_list:
   VAR                           { [$1] }
  | formal_list COMMA VAR         { $3 :: $1 }

vdecl_list:
   /* Nothing */                 { [] }
  | vdecl_list vdecl              { $2 :: $1 }

vdecl:
   VAR SEMI                      { $1 }

stmt_list:
   /* Nothing */                 { [] }
  | stmt_list stmt                { $2 :: $1 }

stmt:
   expr SEMI                     { Expr($1) }
  | RET expr SEMI                 { Ret($2) }
  | LBRACE stmt_list RBRACE       { Block(List.rev($2)) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE
                                  { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt
                                  { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | STOP exc_opt                  { Stop($2) }

exc_opt:
   /* Nothing */                 { [] }
  | exc_list                      { $1 }

exc_list:
   EXC                           { ['!'] }
  | exc_list EXC                  { '!' :: $1 }


expr:
    INTLITERAL                    { IntLiteral($1) }
  | FLOATLITERAL                  { FloatLiteral($1) }
  | STRLITERAL                    { StrLiteral($1) }
  | BOOLLITERAL                   { BoolLiteral($1) }
  | VAR                           { Var($1) }
  | expr PLUS expr                { Binop($1, Add, $3) }
  | expr MINUS expr               { Binop($1, Sub, $3) }
  | expr TIMES expr               { Binop($1, Mult, $3) }
  | expr DIVIDE expr              { Binop($1, Div, $3) }
  | expr EQ expr                  { Binop($1, Eq, $3) }
  | expr NEQ expr                 { Binop($1, Neq, $3) }
  | expr LT expr                  { Binop($1, Lt, $3) }
  | expr LEQ expr                 { Binop($1, Leq, $3) }
  | expr GT expr                  { Binop($1, Gt, $3) }
  | expr GEQ expr                 { Binop($1, Geq, $3) }
  | VAR ASSIGN expr               { Assign($1, $3) }
  | VAR LPAREN actuals_opt RPAREN { Call($1, $3) }
	| LPAREN INT RPAREN LPAREN expr RPAREN        { CastInt($4) }
	| LPAREN BOOLEAN RPAREN LPAREN expr RPAREN    { CastBoolean($4) }
	| LPAREN CHAR RPAREN LPAREN expr RPAREN       { CastChar($4) }
	| LPAREN DOUBLE RPAREN LPAREN expr RPAREN     { CastDouble($4) }
  | LPAREN expr RPAREN            { $2 }

actuals_opt:
   /* Nothing */                 { [] }
  | actuals_list                  { List.rev $1 }

actuals_list:
   expr                          { [$1] }
  | actuals_list COMMA expr       { $3 :: $1 }
