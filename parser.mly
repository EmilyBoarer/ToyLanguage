%{

let anno x = Aast.A(Parsing.symbol_start_pos x)

%}

%token <int> INT
%token <string> IDENT
%token PLUS LTHAN
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMICOLON COLON EQUALS LET
%token WHILE IF ELSE
%token I32_TYPE U32_TYPE
%left LTHAN         /* lowest precedence */
%left PLUS
%nonassoc BRACKETS  /* highest precedence */
%start main         /* the entry point */
%type <Aast.aast> main
%%
main:
  | seq                                 { $1 }
;
seq:
  | LBRACE i RBRACE                     { $2 }
;
i:
  | statement                           { Aast.SEQ(anno(), [$1]) }
  | statement SEMICOLON i               { Aast.SEQ(anno(), [$1; $3]) }
;
statement:
  | expr                                { $1 }
  | IF expr expr ELSE expr              { Aast.IF(anno(), $2, $3, $5) }
  | WHILE expr expr                     { Aast.WHILE(anno(), $2, $3) }
  | LET expr COLON typeexpr EQUALS expr { Aast.LET(anno(), $2, $4, $6) }
  | LET expr COLON typeexpr             { Aast.DECLARE(anno(), $2, $4) }
  | expr EQUALS expr                    { Aast.ASSIGN(anno(), $1, $3) }
;
expr:
  | seq                                 { $1 }
  | INT                                 { Aast.INT(anno(), $1) }
  | IDENT                               { Aast.IDENT(anno(), $1) }
  | LPAREN expr RPAREN %prec BRACKETS   { $2 }
  | expr PLUS expr                      { Aast.INFIX(anno(), $1, Ast.I_ADD, $3) }
  | expr LTHAN expr                     { Aast.INFIX(anno(), $1, Ast.I_LTHAN, $3) }
;
typeexpr:
  | I32_TYPE                            { Aast.TYPE_IDENT(anno(), Ast.I32_T) }
  | U32_TYPE                            { Aast.TYPE_IDENT(anno(), Ast.U32_T) }
;