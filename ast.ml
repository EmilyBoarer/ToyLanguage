
type types =
    | INVALID_T (* if typed to this, then it doesn't type*)
    | UNIT_T
    | U32_T
    | I32_T
    | IDENT_T

type ast =
    | LET of ast * ast * ast (* let x:u32 = 5 *)
    | DECLARE of ast * ast   (* let x:u32 *)
    | ASSIGN of ast * ast    (* x = 5 *)
    | SEQ of (ast list)      (* thing1; thing2; (last semicolon optional)*)
    | INT of int             (* 5 *)
    | INT2 of int             (* TODO REMOVE*)
    | IDENT of string        (* x *)
    | TYPE_IDENT of types    (* u32 *)
(*    | OP2 of ast * ast *)
(*    | PRINT of ast *)

(* TODO: product and sum types (structs and enums) *)

let rec simplify_ast = function (* Convert LET, flatten SEQs *)
    | LET (ast1, ast2, ast3) -> let ast1_ = (simplify_ast ast1) in  (*split let into declare and assign*)
                                SEQ ([
                                    (DECLARE(ast1_, (simplify_ast ast2)));
                                    (ASSIGN (ast1_, (simplify_ast ast3)))
                                ])
    | SEQ ((SEQ(x))::t) -> let t2 = match (simplify_ast (SEQ(t))) with SEQ(t2) -> t2 | _ -> failwith "Error simplifying AST" in (* flatten nested SEQ's into one *)
                           let x2 = match (simplify_ast (SEQ(x))) with SEQ(x2) -> x2 | _ -> failwith "Error simplifying AST" in
                           SEQ(x2@t2)
    | SEQ (h::t) -> let h2 = match (simplify_ast h) with SEQ(h2) -> h2 | h2 -> [h2] in (* recurse over sequence *)
                    let t2 = match (simplify_ast (SEQ(t))) with SEQ(t2) -> t2 | _ -> failwith "Error simplifying AST" in
                    SEQ(h2@t2)
(*    | ASSIGN (ast1, ast2) -> *)
    | x -> x

(* TODO: type inference (ish) // support for multiple types to be valid*)

type var_type = VT of string * types

let rec vt_check = function (* helper function: determine which type is associated with that ident string *)
    | s1, VT(s2, t)::tail -> if s1=s2 then t else vt_check (s1, tail)
    | _, [] -> INVALID_T

let rec type_check = function (* ast node, variable_types*)
    | DECLARE (ast1, ast2), vt -> (* TODO check not already declared under same name! => ct_check -> invalid *)
        (match type_check (ast2, vt) with
            | INVALID_T, _ -> INVALID_T, vt
            | checked_var_type, _ ->
                (match (type_check (ast1, vt)), ast1 with
                    | (IDENT_T,_), IDENT(str) -> UNIT_T, (VT(str, checked_var_type))::vt
                    | _ -> INVALID_T, vt))

    | ASSIGN (ast1, ast2), vt -> (* Assign should return the value being assigned *)
        (match (type_check (ast1, vt)), ast1 with (* check the that identifier has that type associated with it at this point in the program *)
            | (IDENT_T,_), IDENT(str) ->
                let t1, _ = type_check (ast2, vt) in
                (if (vt_check (str, vt)) = t1 then t1 else INVALID_T )
            | _, _ -> INVALID_T), vt
    | SEQ (h::[]), vt -> type_check (h, vt) (* Seq should return (return of the last item in the sequence) *)
    | SEQ (h::t), vt ->
        (match type_check (h, vt) with
            | INVALID_T, _ -> INVALID_T, vt
            | _, vt2 -> type_check (SEQ(t), vt2))
    | INT (_), vt -> U32_T, vt (*TODO this needs to change, INT should be allowed to have multiple types. generally, this returns
                        TODO list of all allowed types? invalid = empty list, some can check that list len = 1, or that a specific item is in the list*)
    | INT2 (_), vt -> I32_T, vt (* TODO REMOVE *)
    | IDENT (_), vt -> IDENT_T, vt
    | TYPE_IDENT (t), vt -> t, vt
    | _, vt -> INVALID_T, vt


let rec simplify_then_type_check = function x -> type_check ((simplify_ast x), [VT("x", U32_T)])