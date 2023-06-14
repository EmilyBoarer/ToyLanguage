
open Aast

(*
TODO somehow store the associated type so asm.ml can use that information to know how to handle each variable
*)

(* TYPE CHECKING ---------------------------------------------------------------------------------------------------- *)

type var_type = VT of string * (Ast.types list)

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


let rec type_check = function (* aast node, variable_types -> acceptable_types list*)
    | DECLARE (a, aast1, aast2), vt ->
        (match type_check (aast2, vt) with
            | [], _ -> Errors.era("ERROR T000: cannot declare without a type", a)
            | checked_var_types, _ ->
                (match aast1 with
                    | IDENT(_, str) -> [Ast.UNIT_T], (VT(str, checked_var_types))::vt
                    | _ -> Errors.era("ERROR T000: cannot assign non-ident", a)))

    | ASSIGN (a, aast1, aast2), vt -> (* Assign should return the value being assigned *)
        (match type_check (aast1, vt) with
            | tl, _ -> (if contains_type (Ast.IDENT_T, tl) then
                (match aast1 with (* check the that identifier has that type associated with it at this point in the program *)
                    | IDENT(_, str) ->
                        let tl2 = (vt_check (str, vt)) in
                        if tl2 = [] then Errors.era("ERROR T000: cannot assign to identifier not in scope", a) else
                        let t1, _ = type_check (aast2, vt) in
                        if t1 = [] then Errors.era("ERROR T000: cannot assign to untyped value", a) else
                        let i = intersection (tl2,t1) in if i = [] then Errors.era("ERROR T000: assigned value doesn't match type", a) else i
                    | _ -> Errors.era("ERROR T000: cannot assign to non-identifier", a)), vt
                else Errors.era("ERROR T000: cannot assign to non-identifier", a)))

    | SEQ (_, h::[]), vt -> type_check (h, vt) (* Seq should return (return of the last item in the sequence) *)
    | SEQ (a, h::t), vt ->
        (match type_check (h, vt) with
            | [], _ -> Errors.era("ERROR T000: sequence item does not type", a)
            | _, vt2 -> type_check (SEQ(a, t), vt2))

    | EVAL (_, aast), vt -> type_check (aast, vt)
    | INT (_, _), vt -> [Ast.I32_T; Ast.U32_T], vt (* allow for multiple different integer representations. TODO check that each int is representable under the given range e.g. disallowing u32 for negative numbers *)
    | BOOL (_, _), vt -> [Ast.BOOL_T], vt (* allow for multiple different integer representations. TODO check that each int is representable under the given range e.g. disallowing u32 for negative numbers *)
    | IDENT (a, str), vt ->
        let ts = vt_check (str, vt) in
        if ts = [] then Errors.era("ERROR T000: ident not declared", a) else IDENT_T::ts, vt
    | TYPE_IDENT (_, t), vt -> [t], vt
    | INFIX (a, aast1, op, aast2), vt -> (match op with
        | I_ADD | I_SUB | I_MULT | I_DIV -> let ts1, _ = type_check (aast1, vt) in
                                            let ts2, _ = type_check (aast2, vt) in
                                            let i1 = intersection ([Ast.U32_T; Ast.I32_T], ts1) in
                                            let i2 = intersection (ts1, ts2) in
                                            if i1 = [] then Errors.era("ERROR T000: infix operand 1's type is not compatible with operator", a)
                                            else if i2 = [] then Errors.era("ERROR T000: infix operand 2's type is not compatible with operand 1's type", a)
                                            else i2, vt
        | I_LTHAN                        -> let ts1, _ = type_check (aast1, vt) in
                                            let ts2, _ = type_check (aast2, vt) in
                                            let i1 = intersection ([Ast.U32_T; Ast.I32_T], ts1) in
                                            let i2 = intersection (ts1, ts2) in
                                            if i1 = [] then Errors.era("ERROR T000: infix operand 1's type is not compatible with operator", a)
                                            else if i2 = [] then Errors.era("ERROR T000: infix operand 2's type is not compatible with operand 1's type", a)
                                            else [BOOL_T], vt
        | I_LSHIFT | I_RSHIFT            -> let ts1, _ = type_check (aast1, vt) in
                                            let ts2, _ = type_check (aast2, vt) in
                                            let i1 = intersection ([Ast.U32_T; Ast.I32_T], ts1) in
                                            let i2 = intersection ([Ast.U32_T], ts2) in
                                            if i1 = [] then Errors.era("ERROR T000: infix operand 1's type is not compatible with operator", a)
                                            else if i2 = [] then Errors.era("ERROR T000: infix operand 2's type is not compatible with operator", a)
                                            else i1, vt
    )
    | IF (a, aast1, aast2, aast3), vt ->
        let ts1, _ = type_check (aast1, vt) in
        let ts2, _ = type_check (aast2, vt) in
        let ts3, _ = type_check (aast3, vt) in
        if (contains_type (Ast.BOOL_T, ts1)) then (
            let i = intersection (ts2, ts3) in
            if i = [] then Errors.era("ERROR T000: if body expressions' types do not match", a)
                      else i, vt
        ) else Errors.era("ERROR T000: if condition doesn't type to boolean", a)
    | WHILE (a, aast1, aast2), vt ->
        let ts1, _ = type_check (aast1, vt) in
        let ts2, _ = type_check (aast2, vt) in
        if (contains_type (Ast.BOOL_T, ts1)) then ts2, vt else Errors.era("ERROR T000: while condition doesn't type to boolean", a)
    | _, vt -> Errors.complain("ERROR T000: unable to type unknown expression")

