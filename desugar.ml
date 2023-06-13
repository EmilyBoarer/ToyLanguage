
open Aast

(*
- Convert LET into DECLARE and ASSIGN
- flatten SEQs,
- EVAL INTs and IDENTs

TODO refactor
TODO give nicer errors
*)

(* DESUGAR ---------------------------------------------------------------------------------------------------------- *)

let rec desugar_aast = function (* aast, bool ('not add eval') -> aast *)
    | LET (aast1, aast2, aast3), _ -> let aast1_ = (desugar_aast (aast1, true)) in  (*split let into declare and assign*)
                                SEQ ([
                                    (DECLARE(aast1_, (desugar_aast (aast2, false))));
                                    (ASSIGN (aast1_, (desugar_aast (aast3, false))))
                                ])
    | SEQ ((SEQ(x))::t), _ -> let t2 = match (desugar_aast (SEQ(t), false)) with SEQ(t2) -> t2 | _ -> failwith "Error simplifying aast" in (* flatten nested SEQ's into one *)
                           let x2 = match (desugar_aast (SEQ(x), false)) with SEQ(x2) -> x2 | _ -> failwith "Error simplifying aast" in
                           SEQ(x2@t2)
    | SEQ (h::t), _ -> let h2 = match (desugar_aast (h, false)) with SEQ(h2) -> h2 | h2 -> [h2] in (* recurse over sequence *)
                    let t2 = match (desugar_aast (SEQ(t), false)) with SEQ(t2) -> t2 | _ -> failwith "Error simplifying aast" in
                    SEQ(h2@t2)
    | ASSIGN (aast1, aast2), _ -> ASSIGN (aast1, desugar_aast (aast2, false))
    | INFIX (aast1, op, aast3), _ -> INFIX(desugar_aast (aast1, true), op, desugar_aast (aast3, true))
    | IF (aast1, aast2, aast3), _ -> IF(desugar_aast (aast1, false), desugar_aast (aast2, false), desugar_aast (aast3, false))
    | WHILE (aast1, aast2), _ -> WHILE(desugar_aast (aast1, false), desugar_aast (aast2, false))
    | INT (aast), false -> EVAL(INT(aast))
    | IDENT (aast), false -> EVAL(IDENT(aast))
    | x, _ -> x
