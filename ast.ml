
type types =
    | UNIT_T
    | IDENT_T
    | U32_T
    | I32_T
    | BOOL_T
    (* if type list is empty -> not typeable *)

type infix_operations =
    | I_ADD
    | I_SUB
    | I_MULT
    | I_DIV
    | I_LSHIFT
    | I_RSHIFT
    | I_LTHAN

type ast =
    | LET of ast * ast * ast (* let x:u32 = 5 *)
    | DECLARE of ast * ast   (* let x:u32 *)
    | ASSIGN of ast * ast    (* x = 5 *)
    | SEQ of (ast list)      (* {thing1; thing2;} (last semicolon optional)*)
    | INT of int             (* 5 *)
    | BOOL of bool           (* true *)
    | IDENT of string        (* x *)
    | TYPE_IDENT of types    (* u32 *)
    | INFIX of ast * infix_operations * ast     (* x+3  or   a<<1   etc..*)
    | EVAL of ast (* TODO: formalise how this is added to the AST, basically, any individual INT or IDENT should be EVALed to get them into register, but without EVAL they make more efficient code in INFIXes *)
    (* TODO: where to handle converting from brackets (3+4)*65 to just nested INFIX *)
    | IF of ast * ast * ast  (* if condition then seq1 else seq2*)
    | WHILE of ast * ast     (* while condition seq *)

(* TODO: product and sum types (structs and enums) *)
