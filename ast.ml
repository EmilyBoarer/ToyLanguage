
type types =
    | UNIT_T
    | U32_T
    | I32_T
    | IDENT_T
    (* if type list is empty -> not typeable *)

type ast =
    | LET of ast * ast * ast (* let x:u32 = 5 *)
    | DECLARE of ast * ast   (* let x:u32 *)
    | ASSIGN of ast * ast    (* x = 5 *)
    | SEQ of (ast list)      (* thing1; thing2; (last semicolon optional)*)
    | INT of int             (* 5 *)
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

(* TODO: type inference*)

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
    | DECLARE (ast1, ast2), vt -> (* TODO check not already declared under same name! => ct_check -> invalid *)
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

    | INT (_), vt -> [I32_T; U32_T], vt (* allow for multiple different integer representations. TODO check that each int is representable under the given range e.g. disallowing u32 for negative numbers *)
    | IDENT (str), vt -> let ts = types_of (str, vt) in
                         IDENT_T::ts, vt
    | TYPE_IDENT (t), vt -> [t], vt
    | _, vt -> [], vt


let simplify_then_type_check = function x -> type_check ((simplify_ast x), [])

(*TODO check each variable is assigned only 1 type in the end! - or at least pick one!*)

let run = simplify_then_type_check ( SEQ([
                                            LET(IDENT("x"), TYPE_IDENT(U32_T), INT(7));
                                            IDENT("x")
                                     ]) )