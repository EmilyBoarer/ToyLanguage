
type aast =
    | LET of aast * aast * aast (* let x:u32 = 5 *)
    | DECLARE of aast * aast   (* let x:u32 *)
    | ASSIGN of aast * aast    (* x = 5 *)
    | SEQ of (aast list)      (* {thing1; thing2;} (last semicolon optional)*)
    | INT of int             (* 5 *)
    | IDENT of string        (* x *)
    | TYPE_IDENT of Ast.types    (* u32 *)
    | INFIX of aast * Ast.infix_operations * aast     (* x+3  or   a<<1   etc..*)
    | EVAL of aast
    | IF of aast * aast * aast  (* if condition then seq1 else seq2*)
    | WHILE of aast * aast     (* while condition seq *)