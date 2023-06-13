
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
    | PRINT of ast           (* print thing  -- prints the value returned by thing *)
    | EVAL of ast (* TODO: formalise how this is added to the AST, basically, any individual INT or IDENT should be EVALed to get them into register, but without EVAL they make more efficient code in INFIXes *)
    (* TODO: where to handle converting from brackets (3+4)*65 to just nested INFIX *)
    | IF of ast * ast * ast  (* if condition then seq1 else seq2*)
    | WHILE of ast * ast     (* while condition seq *)

(* TODO: product and sum types (structs and enums) *)

(* SIMPLIFY AST & DESUGAR ------------------------------------------------------------------------------------------- *)

let rec simplify_ast = function (* Convert LET, flatten SEQs, EVAL INTs and IDENTs :-  ast, bool (not add eval) -> ast *)
    | LET (ast1, ast2, ast3), _ -> let ast1_ = (simplify_ast (ast1, true)) in  (*split let into declare and assign*)
                                SEQ ([
                                    (DECLARE(ast1_, (simplify_ast (ast2, false))));
                                    (ASSIGN (ast1_, (simplify_ast (ast3, false))))
                                ])
    | SEQ ((SEQ(x))::t), _ -> let t2 = match (simplify_ast (SEQ(t), false)) with SEQ(t2) -> t2 | _ -> failwith "Error simplifying AST" in (* flatten nested SEQ's into one *)
                           let x2 = match (simplify_ast (SEQ(x), false)) with SEQ(x2) -> x2 | _ -> failwith "Error simplifying AST" in
                           SEQ(x2@t2)
    | SEQ (h::t), _ -> let h2 = match (simplify_ast (h, false)) with SEQ(h2) -> h2 | h2 -> [h2] in (* recurse over sequence *)
                    let t2 = match (simplify_ast (SEQ(t), false)) with SEQ(t2) -> t2 | _ -> failwith "Error simplifying AST" in
                    SEQ(h2@t2)
    | ASSIGN (ast1, ast2), _ -> ASSIGN (ast1, simplify_ast (ast2, false))
    | INFIX (ast1, op, ast3), _ -> INFIX(simplify_ast (ast1, true), op, simplify_ast (ast3, true))
    | IF (ast1, ast2, ast3), _ -> IF(simplify_ast (ast1, false), simplify_ast (ast2, false), simplify_ast (ast3, false))
    | WHILE (ast1, ast2), _ -> WHILE(simplify_ast (ast1, false), simplify_ast (ast2, false))
    | PRINT (ast), _ -> PRINT(simplify_ast (ast, false))
    | INT (ast), false -> EVAL(INT(ast))
    | IDENT (ast), false -> EVAL(IDENT(ast))
    (* TODO remove SEQ of nothing! *)
    | x, _ -> x

(* TYPE CHECKING ---------------------------------------------------------------------------------------------------- *)

type var_type = VT of string * (types list)

let rec vt_check = function (* helper function: determine which type is associated with that ident string *)
    | s1, VT(s2, t)::tail -> if s1=s2 then t else vt_check (s1, tail)
    | _, [] -> []

let rec intersection2 = function (* helper helper function *)
    | l1, h1::t1, h2::t2 -> let r = intersection2 (l1, t1, h2::t2) in
                            (if h1=h2 then h1::r else r )
    | l1, [], h2::t2 -> intersection2 (l1, l1, t2)
    | _, _, [] -> []
let rec intersection = function (* helper function: intersection of two lists (of types)*)
    l1, l2 -> intersection2 (l1, l1, l2)

let rec contains_type = function (* helper function: returns true iff list of types contains given type *)
    | ty, h::t -> if ty = h then true else contains_type (ty, t)
    | _, [] -> false


let rec type_check = function (* ast node, variable_types -> acceptable_types list*)
    | DECLARE (ast1, ast2), vt ->
        (match type_check (ast2, vt) with
            | [], _ -> [], vt
            | checked_var_types, _ ->
                (match type_check (ast1, vt) with
                    tl, _ -> (if contains_type (IDENT_T, tl) then
                        (match ast1 with
                            | IDENT(str) -> [UNIT_T], (VT(str, checked_var_types))::vt
                            | _ -> [], vt)
                        else [], vt)))

    | ASSIGN (ast1, ast2), vt -> (* Assign should return the value being assigned *)
        (match type_check (ast1, vt) with
            | tl, _ -> (if contains_type (IDENT_T, tl) then
                (match ast1 with (* check the that identifier has that type associated with it at this point in the program *)
                    | IDENT(str) ->
                        let t1, _ = type_check (ast2, vt) in
                        intersection ((vt_check (str, vt)),t1)
                    | _ -> []), vt
                else [], vt))

    | SEQ (h::[]), vt -> type_check (h, vt) (* Seq should return (return of the last item in the sequence) *)
    | SEQ (h::t), vt ->
        (match type_check (h, vt) with
            | [], _ -> [], vt
            | _, vt2 -> type_check (SEQ(t), vt2))

    | EVAL (ast), vt -> type_check (ast, vt)
    | INT (_), vt -> [I32_T; U32_T], vt (* allow for multiple different integer representations. TODO check that each int is representable under the given range e.g. disallowing u32 for negative numbers *)
    | IDENT (str), vt -> let ts = vt_check (str, vt) in
                         IDENT_T::ts, vt
    | TYPE_IDENT (t), vt -> [t], vt
    | PRINT (ast), vt -> let ts, _ = type_check (ast, vt) in
                           if intersection ([U32_T; I32_T], ts) = [] then [], vt
                                                                     else [UNIT_T], vt
    | INFIX (ast1, op, ast2), vt -> (match op with
        | I_ADD | I_SUB | I_MULT | I_DIV -> let ts1, _ = type_check (ast1, vt) in
                                            let ts2, _ = type_check (ast2, vt) in
                                            let i1 = intersection ([U32_T; I32_T], ts1) in
                                            let i2 = intersection (ts1, ts2) in
                                            if i1 = [] then [], vt else if i2 = [] then [], vt else i2, vt
        | I_LTHAN                        -> let ts1, _ = type_check (ast1, vt) in
                                            let ts2, _ = type_check (ast2, vt) in
                                            let i1 = intersection ([U32_T; I32_T], ts1) in
                                            let i2 = intersection (ts1, ts2) in
                                            if i1 = [] then [], vt else if i2 = [] then [], vt else [BOOL_T], vt
        | I_LSHIFT | I_RSHIFT            -> let ts1, _ = type_check (ast1, vt) in
                                            let ts2, _ = type_check (ast2, vt) in
                                            let i1 = intersection ([U32_T; I32_T], ts1) in
                                            let i2 = intersection ([U32_T], ts2) in
                                            if i1 = [] then [], vt else if i2 = [] then [], vt else i1, vt
    )
    | IF (ast1, ast2, ast3), vt ->
        let ts1, _ = type_check (ast1, vt) in
        let ts2, _ = type_check (ast2, vt) in
        let ts3, _ = type_check (ast3, vt) in
        if (contains_type (BOOL_T, ts1)) then (
            let i = intersection (ts2, ts3) in
            if i = [] then [], vt else i, vt
        ) else [], vt
    | WHILE (ast1, ast2), vt ->
        let ts1, _ = type_check (ast1, vt) in
        let ts2, _ = type_check (ast2, vt) in
        if (contains_type (BOOL_T, ts1)) then ts2, vt else [], vt
    | _, vt -> [], vt


(* CONVERT TO PSEUDO ASM -------------------------------------------------------------------------------------------- *)

type asm_instr =
    | ASM_ADD of int * int * int (* rs1, rs2, rd *)
    | ASM_ADDI of int * int * int (* rs1, rd, IMM *)
(*    | ASM_LUI of int * int (* rd, upperIMM *) *)
    | ASM_SLT of int * int * int (* rs1, rs2, rd TODO add SLTU variation*)
    | ASM_SLTI of int * int * int (* rs1, rd, IMM TODO add SLTIU variation*)
    | ASM_JAL of int * int (* rd, LabelRef TODO convert these to relative addresses with another pass later on.*)
    | ASM_BEQ of int * int * int (* rs1, rs2, LabelRef *)
    | ASM_BLT of int * int * int (* rs1, rs2, LabelRef *)
    | ASM_LABEL of int (* LabelRef *)

type var_reg_binding = VRB of string * int (* string of identifier, int of register in register file *)

let rec vrb_lookup = function
    | s, VRB(s2, i)::t -> if s=s2 then i else vrb_lookup (s, t)
    | _, [] -> failwith "ERROR: var reg binding lookup failed"

let rec vrb_assign_helper = function
    | str, vrb, h::t -> if Bool.not (List.exists (function VRB(_,r) -> r=h) vrb)
                        then VRB(str, h)
                        else vrb_assign_helper (str, vrb, t)
    | _, _, [] -> failwith "ERROR: ran out of registers to assign!!"
let vrb_assign = function str, vrb -> vrb_assign_helper (str, vrb, [5;6;7;28;29;30;31])


let new_label_helper = ref 0

let new_label = function () ->  new_label_helper := !new_label_helper+1;
                                (!new_label_helper-1)

    (* TODO: TODO handle when not enough registers push something onto the stack *)

(*
TODO: what about when there aren't enough registers!?
TODO: determine types so know how to treat them in the asm
TODO: how to handle results of infix operations
TODO: chain multiple instructions
*)

type infix_helper = I_H_NONE | I_H_IMM of int | I_H_REG of int

let rec compile = function (* simplified&checked_ast, var/reg bindings, infix_helper -> reversed asm listing *)
    | INFIX(ast1, op, ast2), vrb ->
        let VRB(_,rs1) = vrb_assign("0_rs1", vrb) in
        let instrs1, vrb2, ih1 = compile (ast1, VRB("0_result", rs1)::vrb) in
        let VRB(_,rs2) = vrb_assign("0_rs2", vrb2) in
        let instrs2, _, ih2 = compile (ast2, VRB("0_result", rs2)::vrb) in
        let rd = vrb_lookup ("0_result", vrb) in
        (match op with  (* TODO handle when imm is too long! *)
            | I_ADD -> (match ih1, ih2 with
                                   | I_H_REG(r), I_H_IMM(i) | I_H_IMM(i), I_H_REG(r) ->
                                       instrs1 @ instrs2 @ [ASM_ADDI (r, rd, i)], vrb, I_H_REG(rd)
                                   | I_H_REG(r1), I_H_REG(r2) ->
                                       instrs1 @ instrs2 @ [ASM_ADD (r1, r2, rd)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i1), I_H_IMM(i2) ->
                                       [ASM_ADDI (0,rs1,i1); ASM_ADDI (rs1, rd, i2)], vrb, I_H_REG(rd))
            | I_LTHAN -> (match ih1, ih2 with
                                   | I_H_REG(r), I_H_IMM(i) ->
                                       instrs1 @ [ASM_SLTI (r, rd, i)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i), I_H_REG(r) ->
                                       instrs2 @ [ASM_ADDI (0,rs1,i); ASM_SLT (rs1, r, rd)], vrb, I_H_REG(rd)
                                   | I_H_REG(r1), I_H_REG(r2) ->
                                       instrs1 @ instrs2 @ [ASM_SLT (r1, r2, rd)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i1), I_H_IMM(i2) ->
                                       [ASM_ADDI (0,rs1,i1); ASM_SLTI (rs1, rd, i2)], vrb, I_H_REG(rd))
        )
    | INT(v), vrb ->
        [], vrb, I_H_IMM(v)
    | IDENT(s), vrb ->
        let reg = vrb_lookup (s, vrb) in
        [], vrb, I_H_REG(reg)

    | SEQ(h::[]), vrb -> compile (h, vrb)
    | SEQ(h::t), vrb ->
        let instrs1, vrb2, _ = compile(h, vrb) in
        let instrs2, _, _    = compile(SEQ(t), vrb2) in
        instrs1@instrs2, vrb, I_H_NONE
    | DECLARE(IDENT(s), TYPE_IDENT(I32_T)), vrb -> (* TODO account for type when managing code! *)
        [], (vrb_assign(s, vrb))::vrb, I_H_NONE
    | ASSIGN(IDENT(s), ast), vrb ->
        let rd = vrb_lookup (s, vrb) in
        let instr, _, _ = compile (ast, VRB("0_result", rd)::vrb) in (instr, vrb, I_H_NONE) (* list functions as a stack *)
    | EVAL(INT(v)), vrb ->
        let rd = vrb_lookup ("0_result", vrb) in
        [ASM_ADDI (0, rd, v)], vrb, I_H_NONE
    | EVAL(IDENT(s)), vrb ->
        let rd = vrb_lookup ("0_result", vrb) in
        let rs1 = vrb_lookup (s, vrb) in
        [ASM_ADDI (rs1, rd, 0)], vrb, I_H_NONE

    | IF(INFIX(inf1, op, inf2), ast2, ast3), vrb -> (* special case for infix allows for optimisation! *)
        let instrs2, _, _ = compile(ast2, vrb) in
        let instrs3, _, _ = compile(ast3, vrb) in
        let else_label = new_label() in
        let end_label = new_label() in
        let VRB(_,rs1) = vrb_assign("0_rs1", vrb)  in
        let instrs1_1, vrb2, ih1 = compile (inf1, VRB("0_result", rs1)::vrb) in
        let VRB(_,rs2) = vrb_assign("0_rs1", vrb2) in
        let instrs1_2, _,    ih2 = compile (inf2, VRB("0_result", rs2)::vrb) in
        let instrsB = (match op, ih1, ih2 with
            | I_LTHAN, I_H_REG(r1), I_H_REG(r2) -> [ASM_BLT(r1, r2, else_label)] (* This is the only one of the 4 options that actually yields fewer instructions *)
            | I_LTHAN, I_H_REG(r1), I_H_IMM(i2) -> [ASM_ADDI (0,rs2,i2); ASM_BLT(r1, rs2, else_label)]
            | I_LTHAN, I_H_IMM(i1), I_H_REG(r2) -> [ASM_ADDI (0,rs1,i1); ASM_BLT(rs1, r2, else_label)]
            | I_LTHAN, I_H_IMM(i1), I_H_IMM(i2) -> [ASM_ADDI (0,rs1,i1); ASM_ADDI (0,rs2,i2); ASM_BLT(rs1, rs2, else_label)]
            | _ -> failwith "ERROR: cannot optimise infix if conditional"
        ) in
        instrs1_1 @ instrs1_2 @ instrsB @
        instrs2 @ [ASM_JAL(0, end_label); ASM_LABEL(else_label)] @
        instrs3 @ [ASM_LABEL(end_label)], vrb, I_H_NONE
    | IF(ast1, ast2, ast3), vrb -> (* TODO: what happens when if <> then int(2) else int(5)  -  with respect to multiple allowable types! *)
        let VRB(s,rd2) = vrb_assign("0_result", vrb) in
        let instrs1, _, _ = compile(ast1, VRB(s, rd2)::vrb) in
        let instrs2, _, _ = compile(ast2, vrb) in
        let instrs3, _, _ = compile(ast3, vrb) in
        let else_label = new_label() in
        let end_label = new_label() in
        instrs1 @ [ASM_BEQ(rd2, 0, else_label)] @
        instrs2 @ [ASM_JAL(0, end_label); ASM_LABEL(else_label)] @
        instrs3 @ [ASM_LABEL(end_label)], vrb, I_H_NONE
        (*
        branch condition = false .else
        <then> code
        jal .end, rd=0
        .else
        <else> code
        .end
        *)

    | WHILE(ast1, ast2), vrb ->
            let VRB(s,rd2) = vrb_assign("0_result", vrb) in
            let instrs1, _, _ = compile(ast1, VRB(s, rd2)::vrb) in
            let instrs2, _, _ = compile(ast2, VRB("0_ignore", rd2)::vrb) in
            let start_label = new_label() in
            let end_label = new_label() in
            [ASM_LABEL(start_label)] @ instrs1 @
            [ASM_BEQ(rd2, 0, end_label)] @ instrs2 @
            [ASM_JAL(0, start_label); ASM_LABEL(end_label)], vrb, I_H_NONE
            (*
            .start
            branch condition = false .end
            code
            jal .start, rd=0
            .end
            *)

    | _ -> failwith "Can't compile that yet!"


let compile_ast = function ast -> compile (ast, [VRB("0_result", 10)]) (* TODO: when functions, use of result: a0=10 needs to be re-evaluated *)

(* CALLING CODE ----------------------------------------------------------------------------------------------------- *)

(*
NOTES:
- scope of variables is from whenever they are declared, to the end of the SEQ/block that they are declared in
- if returns the value of whichever branch was evaluated
- while returns the value of the final interation of it's body

MAIN future plan:
- add loops
- add functions
- sort out pushing some values to the stack when running out of registers to hold data in!!
- sort out what happens when something can be typed to multiple things
- then, recursive functions
- then, flesh out the number of implemented infix operators
- custom user types (product and sum types)

- check use before assignment, etc.. other similar safety things, statically

*)

let simplify_then_type_check = function x -> type_check ((simplify_ast (x, false)), [])

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

let run = let code = SEQ([
                            LET(IDENT("a"),TYPE_IDENT(I32_T), INT(0));
                            LET(IDENT("b"),TYPE_IDENT(I32_T), INT(1));
                            DECLARE(IDENT("t"),TYPE_IDENT(I32_T));
                            WHILE( INFIX(IDENT("b"), I_LTHAN, INT(100)),
                                SEQ([
                                     ASSIGN(IDENT("t"), IDENT("b"));
                                     ASSIGN(IDENT("b"), INFIX(IDENT("a"), I_ADD, IDENT("b")));
                                     ASSIGN(IDENT("a"), IDENT("t"));
                                ]));
                            IDENT("b")
                         ]) in
                code,(simplify_ast (code, false)),(simplify_then_type_check code)
                ,(compile_ast (simplify_ast (code, false)))
