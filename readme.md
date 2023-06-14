# Toy Language

## Example: Fibonacci numbers

### source code

```
{
    let a:i32 = 1;
    let b:i32 = 1;
    let t:i32;
    while (b < 100) {
        t = b;
        b = a+b;
        a = t
    };
    b
}
```


### asm

```
.text
main:
li x5, 1
li x6, 1
lab0:
slti x28, x6, 100
beq x28, x0, lab1
mv x7, x6
add x6, x5, x6
mv x5, x7
j lab0
lab1:
mv x10, x6
```

## Use

Compile the compiler from souce with

```./compile-the-compiler.sh```

Compile `file.lang` with

```cat "file.lang" | ./compiler```

The `lang` file extension is an arbitrary choice. 
It simply stands for "language" 

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

> errors.ml

Used in giving generated errors back to the user

## NOTES:
- scope of variables is from whenever they are declared, to the end of the SEQ/block that they are declared in
- if returns the value of whichever branch was evaluated
- while returns the value of the final interation of it's body

## MAIN future plan:
- better errors, that actually tell you what's wrong and where it's wrong
- add shifts and bitwise/logical operations
- add functions
- sort out pushing some values to the stack when running out of registers to hold data in!!
- sort out what happens when something can be typed to multiple things
- then, recursive functions
- then, flesh out the number of implemented infix operators
- custom user types (product and sum types)

- check use before assignment, etc.. other similar safety things, statically

