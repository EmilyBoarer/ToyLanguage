
open Aast

(*
TODO refactor (and rename ast to aast
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


let rec type_check = function (* ast node, variable_types -> acceptable_types list*)
    | DECLARE (ast1, ast2), vt ->
        (match type_check (ast2, vt) with
            | [], _ -> [], vt
            | checked_var_types, _ ->
                (match type_check (ast1, vt) with
                    tl, _ -> (if contains_type (Ast.IDENT_T, tl) then
                        (match ast1 with
                            | IDENT(str) -> [UNIT_T], (VT(str, checked_var_types))::vt
                            | _ -> [], vt)
                        else [], vt)))

    | ASSIGN (ast1, ast2), vt -> (* Assign should return the value being assigned *)
        (match type_check (ast1, vt) with
            | tl, _ -> (if contains_type (Ast.IDENT_T, tl) then
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
(*    | PRINT (ast), vt -> let ts, _ = type_check (ast, vt) in *)
(*                           if intersection ([U32_T; I32_T], ts) = [] then [], vt *)
(*                                                                     else [UNIT_T], vt *)
    | INFIX (ast1, op, ast2), vt -> (match op with
        | I_ADD | I_SUB | I_MULT | I_DIV -> let ts1, _ = type_check (ast1, vt) in
                                            let ts2, _ = type_check (ast2, vt) in
                                            let i1 = intersection ([Ast.U32_T; Ast.I32_T], ts1) in
                                            let i2 = intersection (ts1, ts2) in
                                            if i1 = [] then [], vt else if i2 = [] then [], vt else i2, vt
        | I_LTHAN                        -> let ts1, _ = type_check (ast1, vt) in
                                            let ts2, _ = type_check (ast2, vt) in
                                            let i1 = intersection ([Ast.U32_T; Ast.I32_T], ts1) in
                                            let i2 = intersection (ts1, ts2) in
                                            if i1 = [] then [], vt else if i2 = [] then [], vt else [BOOL_T], vt
        | I_LSHIFT | I_RSHIFT            -> let ts1, _ = type_check (ast1, vt) in
                                            let ts2, _ = type_check (ast2, vt) in
                                            let i1 = intersection ([Ast.U32_T; Ast.I32_T], ts1) in
                                            let i2 = intersection ([Ast.U32_T], ts2) in
                                            if i1 = [] then [], vt else if i2 = [] then [], vt else i1, vt
    )
    | IF (ast1, ast2, ast3), vt ->
        let ts1, _ = type_check (ast1, vt) in
        let ts2, _ = type_check (ast2, vt) in
        let ts3, _ = type_check (ast3, vt) in
        if (contains_type (Ast.BOOL_T, ts1)) then (
            let i = intersection (ts2, ts3) in
            if i = [] then [], vt else i, vt
        ) else [], vt
    | WHILE (ast1, ast2), vt ->
        let ts1, _ = type_check (ast1, vt) in
        let ts2, _ = type_check (ast2, vt) in
        if (contains_type (Ast.BOOL_T, ts1)) then ts2, vt else [], vt
    | _, vt -> [], vt

