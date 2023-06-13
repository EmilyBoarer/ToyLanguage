ocamllex lexer.mll       # generates lexer.ml
ocamlyacc parser.mly     # generates parser.ml and parser.mli
ocamlc -c ast.ml
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c lexerparser.ml
ocamlc -o lexerparser ast.cmo lexer.cmo parser.cmo lexerparser.cmo