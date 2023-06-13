# Toy Language

## Compiler structure

Source code > lexer > tokens

tokens > parser > AAST

AAST > desugarer > AAST

AAST > typechecker > AAST

AAST > dropannotations > AST

AST > many composed transformations > ASM list

ASM list > asmformatter > RISC-V32I assembly code output

## Files:

> compile-the-compiler.sh
 
Run this to generate the compiler from the ocaml source code

> lexer.mll

The input for ocamllex - responsible for lexing

> parser.mly

The input for ocamlyacc - responsible for parsing

> aast.ml

Contains definition of the AAST (Annotated Abstract Syntax Tree)

> desugar.ml

Desugars the AAST

> typer.ml

Type checks the AAST

> ast.ml

Contains definition of the AST

> transform.ml

Contains many transformations on ASTs, and transform AAST to AST

> asm.ml

Do the final conversion from AST to assembly code as text

> compiler.ml

Orchestrates the whole compilation process, passing data from each stage to the next