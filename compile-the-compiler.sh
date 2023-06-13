ocamllex lexer.mll       # generates lexer.ml
ocamlyacc parser.mly     # generates parser.ml and parser.mli
ocamlc -c ast.ml
ocamlc -c aast.ml
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c desugar.mli
ocamlc -c desugar.ml
ocamlc -c typer.ml
ocamlc -c transform.ml
ocamlc -c asm.ml
ocamlc -c compiler.ml
ocamlc -o compiler ast.cmo aast.cmo lexer.cmo parser.cmo desugar.cmo typer.cmo transform.cmo asm.cmo compiler.cmo