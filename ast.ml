
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
    | IDENT of string        (* x *)
    | TYPE_IDENT of types    (* u32 *)
    | INFIX of ast * infix_operations * ast     (* x+3  or   a<<1   etc..*)
    | EVAL of ast (* TODO: formalise how this is added to the AST, basically, any individual INT or IDENT should be EVALed to get them into register, but without EVAL they make more efficient code in INFIXes *)
    (* TODO: where to handle converting from brackets (3+4)*65 to just nested INFIX *)
    | IF of ast * ast * ast  (* if condition then seq1 else seq2*)
    | WHILE of ast * ast     (* while condition seq *)

(* TODO: product and sum types (structs and enums) *)



(* CALLING CODE ----------------------------------------------------------------------------------------------------- *)

(*
NOTES:
- scope of variables is from whenever they are declared, to the end of the SEQ/block that they are declared in
- if returns the value of whichever branch was evaluated
- while returns the value of the final interation of it's body

MAIN future plan:
- add functions
- sort out pushing some values to the stack when running out of registers to hold data in!!
- sort out what happens when something can be typed to multiple things
- then, recursive functions
- then, flesh out the number of implemented infix operators
- custom user types (product and sum types)

- check use before assignment, etc.. other similar safety things, statically

- better errors, that actually tell you what's wrong and where it's wrong
- refactor code for better structuring (not all in this one file)

*)


(* Fibonacci:

{
    let a:u32 = 0;
    let b:u32 = 1;
    let t:u32;
    while b < 100 {
        t = b;
        b = a+b;
        a = t;
    };
    b
}

compiled, then manually transcribed to actual asm: *The order of args is different!!*
    .text
    main:
            addi x5,  x0,  0
            addi x6,  x0,  1
    lab0:   slti x28,  x6, 100
            beq  x28, x0,  lab1
            addi x7,  x6,  0
            add  x6,  x5,  x6
            addi x5,  x7,  0
            jal  x0,       lab0
    lab1:   addi x10,  x6, 0

runs properly and leaves 144 in x10!!
on https://creatorsim.github.io/creator/
*)
(*  *)
(* let run code = *)
(* (*    let code = SEQ([ *) *)
(* (*        LET(IDENT("a"),TYPE_IDENT(I32_T), INT(0)); *) *)
(* (*        LET(IDENT("b"),TYPE_IDENT(I32_T), INT(1)); *) *)
(* (*        DECLARE(IDENT("t"),TYPE_IDENT(I32_T)); *) *)
(* (*        WHILE( INFIX(IDENT("b"), I_LTHAN, INT(100)), *) *)
(* (*            SEQ([ *) *)
(* (*                 ASSIGN(IDENT("t"), IDENT("b")); *) *)
(* (*                 ASSIGN(IDENT("b"), INFIX(IDENT("a"), I_ADD, IDENT("b"))); *) *)
(* (*                 ASSIGN(IDENT("a"), IDENT("t")); *) *)
(* (*            ])); *) *)
(* (*        IDENT("b") *) *)
(* (*        ]) in *) *)
(*    let a = (simplify_ast (code, false)) in *)
(*    let tc = (type_check (a, [])) in *)
(*    let asm, vrb, ih = compile_ast a in *)
(* (*    code,a,tc,(asm, vrb, ih), *) *)
(*    print_asm asm *)
