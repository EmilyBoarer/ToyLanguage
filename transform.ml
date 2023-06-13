

let rec convert_aast_to_ast = function
    | Aast.LET (_,a,b,c)  -> Ast.LET (convert_aast_to_ast a, convert_aast_to_ast b, convert_aast_to_ast c)
    | Aast.DECLARE (_,a,b)-> Ast.DECLARE (convert_aast_to_ast a, convert_aast_to_ast b)
    | Aast.ASSIGN (_,a,b) -> Ast.ASSIGN (convert_aast_to_ast a, convert_aast_to_ast b)
    | Aast.SEQ (_,a)      -> Ast.SEQ (List.map convert_aast_to_ast a)
    | Aast.INT (_,a)      -> Ast.INT (a)
    | Aast.IDENT (_,a)    -> Ast.IDENT (a)
    | Aast.TYPE_IDENT(_,a)-> Ast.TYPE_IDENT (a)
    | Aast.INFIX (_,a,b,c)-> Ast.INFIX (convert_aast_to_ast a, b, convert_aast_to_ast c)
    | Aast.EVAL (_,a)     -> Ast.EVAL (convert_aast_to_ast a)
    | Aast.IF (_,a,b,c)   -> Ast.IF (convert_aast_to_ast a, convert_aast_to_ast b, convert_aast_to_ast c)
    | Aast.WHILE (_,a,b)  -> Ast.WHILE (convert_aast_to_ast a, convert_aast_to_ast b)



let transform = function x -> x