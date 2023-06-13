
type annotation = A of Lexing.position (* starting character position *)

type aast =
    | LET of annotation * aast * aast * aast (* let x:u32 = 5 *)
    | DECLARE of annotation * aast * aast   (* let x:u32 *)
    | ASSIGN of annotation * aast * aast    (* x = 5 *)
    | SEQ of annotation * (aast list)      (* {thing1; thing2;} (last semicolon optional)*)
    | INT of annotation * int             (* 5 *)
    | IDENT of annotation * string        (* x *)
    | TYPE_IDENT of annotation * Ast.types    (* u32 *)
    | INFIX of annotation * aast * Ast.infix_operations * aast     (* x+3  or   a<<1   etc..*)
    | EVAL of annotation * aast
    | IF of annotation * aast * aast * aast  (* if condition then seq1 else seq2*)
    | WHILE of annotation * aast * aast     (* while condition seq *)