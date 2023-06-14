
exception Error of string

let complain s = raise (Error s)

let era = function s, Aast.A(l) -> raise (Error (s^
        (Printf.sprintf "\nFile \"%s\", line %i, char %i" l.Lexing.pos_fname l.Lexing.pos_lnum l.Lexing.pos_bol)))