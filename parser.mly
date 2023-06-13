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
%type <Ast.ast> main
%%
main:
  | seq                                 { $1 }
;
seq:
  | LBRACE i RBRACE                     { $2 }
;
i:
  | statement                           { Ast.SEQ([$1]) }
  | statement SEMICOLON i               { Ast.SEQ([$1; $3]) }
;
statement:
  | expr                                { $1 }
  | IF expr expr ELSE expr              { Ast.IF($2, $3, $5) }
  | WHILE expr expr                     { Ast.WHILE($2, $3) }
  | LET expr COLON typeexpr EQUALS expr { Ast.LET($2, $4, $6) }
  | LET expr COLON typeexpr             { Ast.DECLARE($2, $4) }
  | expr EQUALS expr                    { Ast.ASSIGN($1, $3) }
;
expr:
  | seq                                 { $1 }
  | INT                                 { Ast.INT($1) }
  | IDENT                               { Ast.IDENT($1) }
  | LPAREN expr RPAREN %prec BRACKETS   { $2 }
  | expr PLUS expr                      { Ast.INFIX($1, Ast.I_ADD, $3) }
  | expr LTHAN expr                     { Ast.INFIX($1, Ast.I_LTHAN, $3) }
;
typeexpr:
  | I32_TYPE                            { Ast.TYPE_IDENT(Ast.I32_T) }
  | U32_TYPE                            { Ast.TYPE_IDENT(Ast.U32_T) }
;