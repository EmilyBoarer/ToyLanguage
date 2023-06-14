
open Ast


type asm_instr =
    | ASM_ADD of int * int * int (* rd, rs1, rs2 *)
    | ASM_SUB of int * int * int (* rd, rs1, rs2 *)
    | ASM_ADDI of int * int * int (* rd, rs1, IMM *)
    | ASM_MV of int * int (* rd, rs1 *)
    | ASM_LI of int * int (* rd, IMM *)
    | ASM_SLT of int * int * int (* rd, rs1, rs2 *)
    | ASM_SLTI of int * int * int (* rd, rs1, IMM *)
    | ASM_SEQZ of int * int (* rd, rs1 *)
    | ASM_JAL of int * int (* rd, LabelRef *)
    | ASM_J of int (* LabelRef *)
    | ASM_BEQ of int * int * int (* rs1, rs2, LabelRef *)
    | ASM_BNE of int * int * int (* rs1, rs2, LabelRef *)
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

(*
TODO: what about when there aren't enough registers!?
TODO: determine types so know how to treat them in the asm
*)

type infix_helper = I_H_NONE
    | I_H_IMM of int       (* got from INT() BOOL() : the value of an immediate *)
    | I_H_REG_REF of int   (* got from IDENT() :      a pointer to the register *)
    | I_H_REG of int       (* got from INFIX() :      a pointer to the register, and there is associated code that needs to be used *)

let rec compile = function (* simplified&checked_ast, var/reg bindings, infix_helper -> reversed asm listing *)
    | INFIX(ast1, op, ast2), vrb ->
        let rd = vrb_lookup ("0_result", vrb) in
(*        let VRB(_,rs1) = vrb_assign("0_rs1", vrb) in *)
        let rs1 = rd in (* since will be overwritten by this anyway! *)
        let instrs1, _, ih1 = compile (ast1, VRB("0_result", rs1)::vrb) in
        let VRB(_,rs2) = vrb_assign("0_rs2", vrb) in
        let instrs2, _, ih2 = compile (ast2, VRB("0_result", rs2)::vrb) in
        (match op with  (* TODO handle when imm is too long! *)
            | I_ADD -> (match ih1, ih2 with
                                   | I_H_REG_REF(r), I_H_IMM(i) | I_H_IMM(i), I_H_REG_REF(r) ->
                                       [ASM_ADDI (rd, r, i)], vrb, I_H_REG(rd)
                                   | I_H_REG_REF(r1), I_H_REG_REF(r2) ->
                                       [ASM_ADD (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i1), I_H_IMM(i2) ->
                                       [ASM_LI (rs1, i1); ASM_ADDI (rd, rs1, i2)], vrb, I_H_REG(rd)
                                   | I_H_REG(r), I_H_IMM(i) ->
                                       instrs1 @ [ASM_ADDI (rd, r, i)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i), I_H_REG(r) ->
                                       instrs2 @ [ASM_ADDI (rd, r, i)], vrb, I_H_REG(rd)
                                   | I_H_REG(r1), I_H_REG_REF(r2) ->
                                       instrs1 @ [ASM_ADD (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | I_H_REG_REF(r1), I_H_REG(r2) ->
                                       instrs2 @ [ASM_ADD (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | I_H_REG(r1), I_H_REG(r2) ->
                                       instrs1 @ instrs2 @ [ASM_ADD (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | _, _ -> [], vrb, I_H_NONE ) (* TODO: throw error?? *)
            | I_SUB -> (match ih1, ih2 with
                                   | I_H_REG_REF(r), I_H_IMM(i) ->
                                       [ASM_ADDI (rd, r, -i)], vrb, I_H_REG(rd) (* NOTE: how to handle this negative immediate!?? *)
                                   | I_H_IMM(i), I_H_REG_REF(r) ->
                                       instrs1 @ [ASM_SUB (rd, rs1, r)], vrb, I_H_REG(rd)
                                   | I_H_REG_REF(r1), I_H_REG_REF(r2) ->
                                       [ASM_SUB (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i1), I_H_IMM(i2) ->
                                       [ASM_LI (rs1, i1); ASM_ADDI (rd, rs1, -i2)], vrb, I_H_REG(rd)
                                   | I_H_REG(r), I_H_IMM(i) ->
                                       instrs1 @ [ASM_ADDI (rd, r, -i)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i), I_H_REG(r) ->
                                       instrs2 @ [ASM_SUB (rd, rs1, r)], vrb, I_H_REG(rd)
                                   | I_H_REG(r1), I_H_REG_REF(r2) ->
                                       instrs1 @ [ASM_SUB (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | I_H_REG_REF(r1), I_H_REG(r2) ->
                                       instrs2 @ [ASM_SUB (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | I_H_REG(r1), I_H_REG(r2) ->
                                       instrs1 @ instrs2 @ [ASM_SUB (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | _, _ -> [], vrb, I_H_NONE ) (* TODO: throw error?? *)
            | I_LTHAN -> (match ih1, ih2 with
                                   | I_H_REG_REF(r), I_H_IMM(i) ->
                                       [ASM_SLTI (rd, r, i)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i), I_H_REG_REF(r) ->
                                       [ASM_LI (rs1, i); ASM_SLT (rd, rs1, r)], vrb, I_H_REG(rd)
                                   | I_H_REG_REF(r1), I_H_REG_REF(r2) ->
                                       [ASM_SLT (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i1), I_H_IMM(i2) ->
                                       [ASM_LI (rs1, i1); ASM_SLTI (rd, rs1, i2)], vrb, I_H_REG(rd)
                                   | I_H_REG(r), I_H_IMM(i) ->
                                       instrs1 @ [ASM_SLTI (rd, r, i)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i), I_H_REG(r) ->
                                       instrs2 @ [ASM_LI (rs1, i); ASM_SLT (rd, rs1, r)], vrb, I_H_REG(rd)
                                   | I_H_REG(r1), I_H_REG_REF(r2) ->
                                       instrs1 @ [ASM_SLT (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | I_H_REG_REF(r1), I_H_REG(r2) ->
                                       instrs2 @ [ASM_SLT (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | I_H_REG(r1), I_H_REG(r2) ->
                                       instrs1 @ instrs2 @ [ASM_SLT (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | _, _ -> [], vrb, I_H_NONE ) (* TODO: throw error?? *)
            | I_EQUAL -> (match ih1, ih2 with (* == -> sub, =0? TODO: optimisation rs1 = rd? rather than allocating a new rs1 ??*)
                                   | I_H_REG_REF(r), I_H_IMM(i) | I_H_IMM(i), I_H_REG_REF(r) ->
                                       [ASM_ADDI (rd, r, -i); ASM_SEQZ (rd, rd)], vrb, I_H_REG(rd) (* subtraction order doesn't make a difference, since signed operation *)
                                   | I_H_REG_REF(r1), I_H_REG_REF(r2) ->
                                       [ASM_SUB (rd, r1, r2); ASM_SEQZ (rd, rd)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i1), I_H_IMM(i2) ->
                                       [ASM_LI (rs1, i1); ASM_ADDI (rd, rs1, -i2); ASM_SEQZ (rd, rd)], vrb, I_H_REG(rd)
                                   | I_H_REG(r), I_H_IMM(i) ->
                                       instrs1 @ [ASM_ADDI (rd, r, -i); ASM_SEQZ (rd, rd)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i), I_H_REG(r) ->
                                       instrs2 @ [ASM_SUB (rd, rs1, r); ASM_SEQZ (rd, rd)], vrb, I_H_REG(rd)
                                   | I_H_REG(r1), I_H_REG_REF(r2) ->
                                       instrs1 @ [ASM_SUB (rd, r1, r2); ASM_SEQZ (rd, rd)], vrb, I_H_REG(rd)
                                   | I_H_REG_REF(r1), I_H_REG(r2) ->
                                       instrs2 @ [ASM_SUB (rd, r1, r2); ASM_SEQZ (rd, rd)], vrb, I_H_REG(rd)
                                   | I_H_REG(r1), I_H_REG(r2) ->
                                       instrs1 @ instrs2 @ [ASM_SUB (rd, r1, r2); ASM_SEQZ (rd, rd)], vrb, I_H_REG(rd)
                                   | _, _ -> [], vrb, I_H_NONE ) (* TODO: throw error?? *)
        )
    | INT(v), vrb ->
        let rd = vrb_lookup ("0_result", vrb) in
        [ASM_LI (rd, v)], vrb, I_H_IMM(v)
    | BOOL(v), vrb ->
        let rd = vrb_lookup ("0_result", vrb) in
        let b = (match v with true -> 1 | false -> 0) in
        [ASM_LI (rd, b)], vrb, I_H_IMM(b)
    | IDENT(s), vrb ->
        let rd = vrb_lookup ("0_result", vrb) in
        let rs1 = vrb_lookup (s, vrb) in
        [ASM_MV (rd, rs1)], vrb, I_H_REG_REF(rs1)

    | SEQ(h::[]), vrb -> compile (h, vrb)
    | SEQ(h::t), vrb ->
        let instrs1, vrb2, _ = compile(h, vrb) in
        let instrs2, _, _    = compile(SEQ(t), vrb2) in
        instrs1@instrs2, vrb, I_H_NONE
    | DECLARE(IDENT(s), TYPE_IDENT(_)), vrb -> (* TODO account for type when managing code! *)
        [], (vrb_assign(s, vrb))::vrb, I_H_NONE
    | ASSIGN(IDENT(s), ast), vrb ->
        let rd = vrb_lookup (s, vrb) in
        let instr, _, _ = compile (ast, VRB("0_result", rd)::vrb) in (instr, vrb, I_H_NONE) (* list functions as a stack *)

    | IF(ast1, ast2, ast3), vrb -> (* TODO: what happens when if <> then int(2) else int(5)  -  with respect to multiple allowable types! *)
        (*conditional results can be stored in rd, since rd will be overwritten by the body anyway. saves allocating another register*)
        let rd = vrb_lookup ("0_result", vrb) in
        let instrs2, _, _ = compile(ast2, vrb) in
        let instrs3, _, _ = compile(ast3, vrb) in
        let else_label = new_label() in
        let end_label = new_label() in
        (compile_optimised_infix_conditional vrb rd ast1 else_label) @
        instrs2 @ [ASM_J(end_label); ASM_LABEL(else_label)] @
        instrs3 @ [ASM_LABEL(end_label)], vrb, I_H_NONE
        (*
        branch not condition .else
        <then> code
        jal .end, rd=0
        .else
        <else> code
        .end
        *)

    | WHILE(ast1, ast2), vrb ->
        let rd = vrb_lookup ("0_result", vrb) in
        let instrs2, _, _ = compile(ast2, vrb) in
        let start_label = new_label() in
        let end_label = new_label() in
        [ASM_LABEL(start_label)] @
        (compile_optimised_infix_conditional vrb rd ast1 end_label) @
        instrs2 @ [ASM_J(start_label); ASM_LABEL(end_label)], vrb, I_H_NONE
        (*
        .start
        branch not condition .end
        code
        jal .start, rd=0
        .end
        *)

    | _ -> failwith "Can't compile that yet!"

and compile_optimised_infix_conditional vrb rd ast unsatisfied_label = (match ast with
    | INFIX(ast1,I_LTHAN,ast2) ->
        let rs1 = rd in
        let instrs1, _, ih1 = compile(ast1, VRB("0_result", rs1)::vrb) in
        let VRB(_,rs2) = vrb_assign("0_rs2", vrb) in
        let instrs2, _, ih2 = compile(ast2, VRB("0_result", rs2)::vrb) in
        (match ih1 with
            | I_H_REG_REF(r1) -> (match ih2 with
                | I_H_REG_REF(r2) -> [ASM_BLT (r2, r1, unsatisfied_label)]
                | _ -> instrs2 @ [ASM_BLT (rs2, r1, unsatisfied_label)])
            | _ -> instrs1 @ instrs2 @ [ASM_BLT (rs2, rs1, unsatisfied_label)])
    | INFIX(ast1,I_EQUAL,ast2) ->
        let rs1 = rd in
        let instrs1, _, ih1 = compile(ast1, VRB("0_result", rs1)::vrb) in
        let VRB(_,rs2) = vrb_assign("0_rs2", vrb) in
        let instrs2, _, ih2 = compile(ast2, VRB("0_result", rs2)::vrb) in
        (match ih1 with
            | I_H_REG_REF(r1) -> (match ih2 with
                | I_H_REG_REF(r2) -> [ASM_BNE(r1, r2, unsatisfied_label)]
                | _ -> instrs2 @ [ASM_BNE(r1, rs2, unsatisfied_label)])
            | _ -> instrs1 @ instrs2 @ [ASM_BNE(rs1, rs2, unsatisfied_label)])
    | _ ->
        let rs1 = rd in
        let instrs1, _, _ = compile(ast, vrb) in
        instrs1 @ [ASM_BEQ(rs1, 0, unsatisfied_label)])



let compile_ast = function ast -> compile (ast, [VRB("0_result", 10)]) (* TODO: when functions, use of result: a0=10 needs to be re-evaluated *)






let rec print_asm_helper = function
    | ASM_ADDI(rd, rs1, imm)::t ->
        Printf.printf "addi x%i, x%i, %i\n" rd rs1 imm;
        print_asm_helper t
    | ASM_MV(rd, rs1)::t ->
        Printf.printf "mv x%i, x%i\n" rd rs1;
        print_asm_helper t
    | ASM_LI(rd, imm)::t ->
        Printf.printf "li x%i, %i\n" rd imm;
        print_asm_helper t
    | ASM_ADD(rd, rs1, rs2)::t ->
        Printf.printf "add x%i, x%i, x%i\n" rd rs1 rs2;
        print_asm_helper t
    | ASM_SUB(rd, rs1, rs2)::t ->
        Printf.printf "sub x%i, x%i, x%i\n" rd rs1 rs2;
        print_asm_helper t
    | ASM_SLTI(rd, rs1, imm)::t ->
        Printf.printf "slti x%i, x%i, %i\n" rd rs1 imm;
        print_asm_helper t
    | ASM_SLT(rd, rs1, rs2)::t ->
        Printf.printf "slt x%i, x%i, x%i\n" rd rs1 rs2;
        print_asm_helper t
    | ASM_SEQZ(rd, rs1)::t ->
        Printf.printf "seqz x%i, x%i\n" rd rs1;
        print_asm_helper t
    | ASM_BEQ(rs1, rs2, label)::t ->
        Printf.printf "beq x%i, x%i, lab%i\n" rs1 rs2 label;
        print_asm_helper t
    | ASM_BNE(rs1, rs2, label)::t ->
        Printf.printf "bne x%i, x%i, lab%i\n" rs1 rs2 label;
        print_asm_helper t
    | ASM_BLT(rs1, rs2, label)::t ->
        Printf.printf "blt x%i, x%i, lab%i\n" rs1 rs2 label;
        print_asm_helper t
    | ASM_JAL(rd, label)::t ->
        Printf.printf "jal x%i, lab%i\n" rd label;
        print_asm_helper t
    | ASM_J(label)::t ->
        Printf.printf "j lab%i\n" label;
        print_asm_helper t
    | ASM_LABEL(label)::t ->
        Printf.printf "lab%i:\n" label;
        print_asm_helper t
    | [] -> ()


let print_asm = function l ->
    print_endline ".text";
    print_endline "main:" ;
    print_asm_helper l