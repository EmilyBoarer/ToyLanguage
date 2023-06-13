

let rec convert_aast_to_ast = function
    | Aast.LET (a,b,c)  -> Ast.LET (convert_aast_to_ast a, convert_aast_to_ast b, convert_aast_to_ast c)
    | Aast.DECLARE (a,b)-> Ast.DECLARE (convert_aast_to_ast a, convert_aast_to_ast b)
    | Aast.ASSIGN (a,b) -> Ast.ASSIGN (convert_aast_to_ast a, convert_aast_to_ast b)
    | Aast.SEQ (a)      -> Ast.SEQ (List.map convert_aast_to_ast a)
    | Aast.INT (a)      -> Ast.INT (a)
    | Aast.IDENT (a)    -> Ast.IDENT (a)
    | Aast.TYPE_IDENT(a)-> Ast.TYPE_IDENT (a)
    | Aast.INFIX (a,b,c)-> Ast.INFIX (convert_aast_to_ast a, b, convert_aast_to_ast c)
    | Aast.EVAL (a)     -> Ast.EVAL (convert_aast_to_ast a)
    | Aast.IF (a,b,c)   -> Ast.IF (convert_aast_to_ast a, convert_aast_to_ast b, convert_aast_to_ast c)
    | Aast.WHILE (a,b)  -> Ast.WHILE (convert_aast_to_ast a, convert_aast_to_ast b)



let transform = function x -> x