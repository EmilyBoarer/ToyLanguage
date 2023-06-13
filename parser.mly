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
  | statement                           { Aast.SEQ([$1]) }
  | statement SEMICOLON i               { Aast.SEQ([$1; $3]) }
;
statement:
  | expr                                { $1 }
  | IF expr expr ELSE expr              { Aast.IF($2, $3, $5) }
  | WHILE expr expr                     { Aast.WHILE($2, $3) }
  | LET expr COLON typeexpr EQUALS expr { Aast.LET($2, $4, $6) }
  | LET expr COLON typeexpr             { Aast.DECLARE($2, $4) }
  | expr EQUALS expr                    { Aast.ASSIGN($1, $3) }
;
expr:
  | seq                                 { $1 }
  | INT                                 { Aast.INT($1) }
  | IDENT                               { Aast.IDENT($1) }
  | LPAREN expr RPAREN %prec BRACKETS   { $2 }
  | expr PLUS expr                      { Aast.INFIX($1, Ast.I_ADD, $3) }
  | expr LTHAN expr                     { Aast.INFIX($1, Ast.I_LTHAN, $3) }
;
typeexpr:
  | I32_TYPE                            { Aast.TYPE_IDENT(Ast.I32_T) }
  | U32_TYPE                            { Aast.TYPE_IDENT(Ast.U32_T) }
;