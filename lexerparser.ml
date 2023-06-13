(* https://v2.ocaml.org/manual/lexyacc.html#s%3Alexyacc-example *)

let convert = function Ast.SEQ(_) -> 1 |_ -> 0

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
      let result = Parser.main Lexer.token lexbuf in
        print_int (convert result); print_newline(); flush stdout
  with Lexer.Eof ->
    exit 0