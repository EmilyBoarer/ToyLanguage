
open Ast


type asm_instr =
    | ASM_ADD of int * int * int (* rd, rs1, rs2 *)
    | ASM_ADDI of int * int * int (* rd, rs1, IMM *)
    | ASM_SLT of int * int * int (* rd, rs1, rs2 TODO add SLTU variation*)
    | ASM_SLTI of int * int * int (* rd, rs1, IMM TODO add SLTIU variation*)
    | ASM_JAL of int * int (* rd, LabelRef *)
    | ASM_BEQ of int * int * int (* rs1, rs2, LabelRef *)
    | ASM_BGE of int * int * int (* rs1, rs2, LabelRef *)
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
                                       instrs1 @ instrs2 @ [ASM_ADDI (rd, r, i)], vrb, I_H_REG(rd)
                                   | I_H_REG(r1), I_H_REG(r2) ->
                                       instrs1 @ instrs2 @ [ASM_ADD (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i1), I_H_IMM(i2) ->
                                       [ASM_ADDI (rs1, 0, i1); ASM_ADDI (rd, rs1, i2)], vrb, I_H_REG(rd)
                                   | _, _ -> [], vrb, I_H_NONE ) (* TODO: throw error?? *)
            | I_LTHAN -> (match ih1, ih2 with
                                   | I_H_REG(r), I_H_IMM(i) ->
                                       instrs1 @ [ASM_SLTI (rd, r, i)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i), I_H_REG(r) ->
                                       instrs2 @ [ASM_ADDI (rs1, 0, i); ASM_SLT (rd, rs1, r)], vrb, I_H_REG(rd)
                                   | I_H_REG(r1), I_H_REG(r2) ->
                                       instrs1 @ instrs2 @ [ASM_SLT (rd, r1, r2)], vrb, I_H_REG(rd)
                                   | I_H_IMM(i1), I_H_IMM(i2) ->
                                       [ASM_ADDI (rs1, 0, i1); ASM_SLTI (rd, rs1, i2)], vrb, I_H_REG(rd)
                                   | _, _ -> [], vrb, I_H_NONE ) (* TODO: throw error?? *)
            | _ -> [], vrb, I_H_NONE (* TODO: implement!*)
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
        [ASM_ADDI (rd, 0, v)], vrb, I_H_NONE
    | EVAL(IDENT(s)), vrb ->
        let rd = vrb_lookup ("0_result", vrb) in
        let rs1 = vrb_lookup (s, vrb) in
        [ASM_ADDI (rd, rs1, 0)], vrb, I_H_NONE

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
            | I_LTHAN, I_H_REG(r1), I_H_REG(r2) -> [ASM_BGE(r1, r2, else_label)] (* This is the only one of the 4 options that actually yields fewer instructions *)
            | I_LTHAN, I_H_REG(r1), I_H_IMM(i2) -> [ASM_ADDI (rs2, 0, i2); ASM_BGE(r1, rs2, else_label)]
            | I_LTHAN, I_H_IMM(i1), I_H_REG(r2) -> [ASM_ADDI (rs1, 0, i1); ASM_BGE(rs1, r2, else_label)]
            | I_LTHAN, I_H_IMM(i1), I_H_IMM(i2) -> [ASM_ADDI (rs1, 0, i1); ASM_ADDI (rs2, 0, i2); ASM_BGE(rs1, rs2, else_label)]
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






let rec print_asm_helper = function
    | ASM_ADDI(rd, rs1, imm)::t ->
        Printf.printf "addi x%i, x%i, %i\n" rd rs1 imm;
        print_asm_helper t
    | ASM_ADD(rd, rs1, rs2)::t ->
        Printf.printf "add x%i, x%i, x%i\n" rd rs1 rs2;
        print_asm_helper t
    | ASM_SLTI(rd, rs1, imm)::t ->
        Printf.printf "slti x%i, x%i, %i\n" rd rs1 imm;
        print_asm_helper t
    | ASM_SLT(rd, rs1, rs2)::t ->
        Printf.printf "slt x%i, x%i, x%i\n" rd rs1 rs2;
        print_asm_helper t
    | ASM_BGE(rs1, rs2, label)::t ->
        Printf.printf "bge x%i, x%i, lab%i\n" rs1 rs2 label;
        print_asm_helper t
    | ASM_BEQ(rs1, rs2, label)::t ->
        Printf.printf "beq x%i, x%i, lab%i\n" rs1 rs2 label;
        print_asm_helper t
    | ASM_JAL(rd, label)::t ->
        Printf.printf "jal x%i, lab%i\n" rd label;
        print_asm_helper t
    | ASM_LABEL(label)::t ->
        Printf.printf "lab%i:\n" label;
        print_asm_helper t
    | [] -> ()


let print_asm = function l ->
    print_endline ".text";
    print_endline "main:" ;
    print_asm_helper l