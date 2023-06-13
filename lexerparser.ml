(* https://v2.ocaml.org/manual/lexyacc.html#s%3Alexyacc-example *)


let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
      let result = Parser.main Lexer.token lexbuf in
        print_int result; print_newline(); flush stdout
  with Lexer.Eof ->
    exit 0