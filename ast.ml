
type types =
    | UNIT_T
    | U32_T
    | I32_T
    | IDENT_T
    (* if type list is empty -> not typeable *)

type infix_operations =
    | I_ADD
    | I_SUB
    | I_MULT
    | I_DIV
    | I_LSHIFT
    | I_RSHIFT

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

(* TODO: product and sum types (structs and enums) *)

(* SIMPLIFY AST & DESUGAR ------------------------------------------------------------------------------------------- *)

let rec simplify_ast = function (* Convert LET, flatten SEQs :-  ast, bool (not add eval) -> ast *)
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
    | INFIX (ast1, ast2, ast3), _ -> INFIX(simplify_ast (ast1, true), ast2, simplify_ast (ast3, true))
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

let rec types_of = function (* helper function: return list of all types of identifier with name str *)
    | str, VT(s, ts)::t -> if str = s then ts else types_of (str, t)
    | _, [] -> []

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
    | IDENT (str), vt -> let ts = types_of (str, vt) in
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
        | I_LSHIFT | I_RSHIFT            -> let ts1, _ = type_check (ast1, vt) in
                                            let ts2, _ = type_check (ast2, vt) in
                                            let i1 = intersection ([U32_T; I32_T], ts1) in
                                            let i2 = intersection ([U32_T], ts2) in
                                            if i1 = [] then [], vt else if i2 = [] then [], vt else i1, vt
    )
    | _, vt -> [], vt


(* CONVERT TO PSEUDO ASM -------------------------------------------------------------------------------------------- *)

type asm_instr =
    | ASM_ADD of int * int * int (* rs1, rs2, rd *)
    | ASM_ADDI of int * int * int (* rs1, rd, IMM *)
    | ASM_LUI of int * int (* rd, upperIMM *)

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

    (* TODO: TODO handle when not enough registers push something onto the stack *)

(*
TODO: what about when there aren't enough registers!?
TODO: determine types so know how to treat them in the asm
TODO: how to handle results of infix operations
TODO: chain multiple instructions
*)

type infix_helper = I_H_NONE | I_H_IMM of int | I_H_REG of int

let rec compile = function (* simplified&checked_ast, var/reg bindings, infix_helper -> reversed asm listing *)
    | INFIX(ast1, I_ADD, ast2), vrb ->
        let VRB(_,rs1) = vrb_assign("0_rs1", vrb) in
        let instrs1, vrb2, ih1 = compile (ast1, VRB("0_result", rs1)::vrb) in
        let VRB(_,rs2) = vrb_assign("0_rs2", vrb2) in
        let instrs2, _, ih2 = compile (ast2, VRB("0_result", rs2)::vrb) in
        let rd = vrb_lookup ("0_result", vrb) in
        (match ih1, ih2 with
            | I_H_REG(r), I_H_IMM(i) | I_H_IMM(i), I_H_REG(r) -> (* TODO handle when imm is too long! *)
                instrs1 @ instrs2 @ [ASM_ADDI (r, rd, i)], vrb, I_H_REG(rd)
            | I_H_REG(r1), I_H_REG(r2) ->
                instrs1 @ instrs2 @ [ASM_ADD (r1, r2, rd)], vrb, I_H_REG(rd)
            | I_H_IMM(i1), I_H_IMM(i2) ->
                [ASM_ADDI (0,rs1,i1); ASM_ADDI (rs1, rd, i2)], vrb, I_H_REG(rd)
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
    | _ -> failwith "Can't compile that yet!"


let compile_ast = function ast -> compile (ast, [VRB("0_result", 10)]) (* TODO: when functions, use of result: a0=10 needs to be re-evaluated *)

(* CALLING CODE ----------------------------------------------------------------------------------------------------- *)

(*
NOTES:
- scope of variables is from whenever they are declared, to the end of the SEQ/block that they are declared in

*)

let simplify_then_type_check = function x -> type_check ((simplify_ast (x, false)), [])


(* let run = let code = SEQ([ *)
(*                         LET(IDENT("x"), TYPE_IDENT(I32_T), INT(7)); *)
(*                         PRINT(SEQ([ *)
(*                                       LET(IDENT("Y"), TYPE_IDENT(I32_T), INT(3)); *)
(*                                       INFIX(IDENT("x"), I_ADD, IDENT("Y")) *)
(*                               ])) *)
(*                  ]) in code,(simplify_ast code),(simplify_then_type_check code),(compile_ast (simplify_ast code)) *)


let run = let code = SEQ([
                            LET(IDENT("x"),TYPE_IDENT(I32_T), INT(60));
                            LET(IDENT("y"),TYPE_IDENT(I32_T), INFIX(INT(90), I_ADD, INT(10)));
                            INFIX(
                                INFIX(IDENT("x"), I_ADD, INT(300))
                                , I_ADD, IDENT("y"))
                         ]) in
                code,(simplify_ast (code, false)),(simplify_then_type_check code)
                ,(compile_ast (simplify_ast (code, false)))


