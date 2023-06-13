%token <int> INT
%token <int> IDENT
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
%type <int> main // TODO need to do something about this return type??
%%
main:
  | seq                                 { $1 }
;
seq:
  | LBRACE i RBRACE                     { $2 }
;
i:
  | statement                           { $1 }
  | statement SEMICOLON i               { $3 }
;
statement:
  | expr                                { $1 }
  | IF expr seq ELSE seq                { $2 } /* TODO convert to IF(..) */
  | WHILE expr seq                      { $2 } /* TODO convert to WHILE(..) */
  | LET expr COLON typeexpr EQUALS expr { $2 } /* TODO convert to LET(..) */
  | LET expr COLON typeexpr             { $2 } /* TODO convert to DECLARE(..) */
  | expr EQUALS expr                    { $3 } /* TODO convert to ASSIGN(..) */
;
expr:
  | seq                                 { $1 }
  | INT                                 { $1 }
  | LPAREN expr RPAREN %prec BRACKETS   { $2 }
  | expr PLUS expr                      { $3 }
  | expr LTHAN expr                     { $3 }
;
typeexpr:
  | I32_TYPE                            { 0 }
  | U32_TYPE                            { 0 }
;