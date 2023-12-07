# Yacc & Lisp Interpreter

Interpreter outputs are written in the file "output.txt" and the generated symbol table is printed on the terminal screen.

## Yacc Interpreter
- make
- ./gpp_interpreter input1.gpp (read from file "input1.gpp")
- /gpp_interpreter (read from shell)

## Lisp Interpreter
clisp gpp_interpreter.lisp input1.gpp (read from file "input1.gpp")
clisp gpp_interpreter.lisp  (read from shell)

### Done ✓

- [x] EXP (+, -, *, /, >, =)
- [x] Binary values and expressions (and, or, not) 
- [x] Control Statements (If, while)
- [x] Symbol Table Implementation
- [x] Definitions (defvar, deffun)
- [x] Assigment (set)

### In Progress

- [ ] Parse Tree implementation (Almost done in Lisp)  
- [ ] Function Call (Since parse tree is not ready, It's not ready yet)
