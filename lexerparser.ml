(* https://v2.ocaml.org/manual/lexyacc.html#s%3Alexyacc-example *)

(* let rec pretty_print_ast = function *)
(*    | Ast.SEQ([]) -> () *)
(*    | Ast.SEQ(h::t) ->  print_string "SEQ(";pretty_print_ast h; *)
(*                        print_string ","; pretty_print_ast (Ast.SEQ(t)); print_string ")" *)
(*    | Ast.INT(i) -> print_string (string_of_int i) *)
(*    | Ast.IDENT(i) -> print_string i *)
(*    | Ast.IF(a,b,c) ->  print_string "IF("; pretty_print_ast a; *)
(*                        print_string ","; pretty_print_ast b; *)
(*                        print_string ","; pretty_print_ast c; print_string ")" *)
(*  *)
(*    | _ -> print_string "<ppunknown>" *)

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
      let result = Parser.main Lexer.token lexbuf in
        Ast.run result; print_newline(); flush stdout
  with Lexer.Eof ->
    exit 0