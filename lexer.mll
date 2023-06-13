{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
  | '+'            { PLUS }
(*  | '-'            { MINUS } *)
(*  | '*'            { TIMES } *)
(*  | '/'            { DIV } *)
  | '<'            { LTHAN }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | ';'            { SEMICOLON }
  | '='            { EQUALS }
  | "let"          { LET }
  | ':'            { COLON }
  | "i32"          { I32_TYPE }
  | "u32"          { U32_TYPE }
  | "while"        { WHILE }
  | "if"           { IF }
  | "else"         { ELSE }

  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | ['a'-'z']+ as lxm { IDENT(int_of_string lxm) }

  | eof            { raise Eof }