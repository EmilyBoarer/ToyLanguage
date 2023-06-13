(* https://v2.ocaml.org/manual/lexyacc.html#s%3Alexyacc-example *)

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
      let aast1 = Parser.main Lexer.token lexbuf in
        let aast2 = Desugar.desugar_aast (aast1, false) in
          let _ = Typer.type_check (aast2, []) in
            let ast = Transform.convert_aast_to_ast aast2 in
              let ast2 = Transform.transform ast in
                let asm, _, _ = Asm.compile_ast ast2 in
                  Asm.print_asm asm; print_newline(); flush stdout
  with Lexer.Eof ->
    exit 0