
open Aast

(*
- Convert LET into DECLARE and ASSIGN
- flatten SEQs,
- EVAL INTs and IDENTs

TODO refactor
TODO give nicer errors
*)

let rec desugar_aast = function (* aast, bool ('not add eval') -> aast *)
    | LET (a, aast1, aast2, aast3), _ -> let aast1_ = (desugar_aast (aast1, true)) in  (*split let into declare and assign*)
                                SEQ (a, [
                                    (DECLARE(a, aast1_, (desugar_aast (aast2, false))));
                                    (ASSIGN (a, aast1_, (desugar_aast (aast3, false))))
                                ])
    | SEQ (a, (SEQ(a2, x))::t), _ -> let t2 = match (desugar_aast (SEQ(a, t), false)) with SEQ(a, t2) -> t2 | _ -> failwith "Error simplifying aast" in (* flatten nested SEQ's into one *)
                           let x2 = match (desugar_aast (SEQ(a2, x), false)) with SEQ(a, x2) -> x2 | _ -> failwith "Error simplifying aast" in
                           SEQ(a, x2@t2)
    | SEQ (a, h::t), _ -> let h2 = match (desugar_aast (h, false)) with SEQ(a, h2) -> h2 | h2 -> [h2] in (* recurse over sequence *)
                    let t2 = match (desugar_aast (SEQ(a, t), false)) with SEQ(a, t2) -> t2 | _ -> failwith "Error simplifying aast" in
                    SEQ(a, h2@t2)
    | ASSIGN (a, aast1, aast2), _ -> ASSIGN (a, aast1, desugar_aast (aast2, false))
    | INFIX (a, aast1, op, aast3), _ -> INFIX(a, desugar_aast (aast1, true), op, desugar_aast (aast3, true))
    | IF (a, aast1, aast2, aast3), _ -> IF(a, desugar_aast (aast1, false), desugar_aast (aast2, false), desugar_aast (aast3, false))
    | WHILE (a, aast1, aast2), _ -> WHILE(a, desugar_aast (aast1, false), desugar_aast (aast2, false))
    | INT (a, x), false -> EVAL(a, INT(a, x))
    | BOOL (a, x), false -> EVAL(a, BOOL(a, x))
    | IDENT (a, s), false -> EVAL(a, IDENT(a, s))
    | x, _ -> x
