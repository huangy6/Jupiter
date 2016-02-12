# Jupiter
An interpreter written in Scheme for Connamacherese, a Java/C-like language. 

## Authors
Jack Mosseau, Vimig Socrates, and Yidi Huang

## Usage
```
git clone https://github.com/jmosseau/jupiter
(mit-scheme) => (load "jupiter.scm")
(mit-scheme) => (interpret "myprogram.cme")
```

## Language features
* Integer arithmetic
  - Unary operations
    + negation
  - Binary operations
    + addition
    + subtraction
    + multiplication
    + division
    + modulo
* Boolean logic
  - Unary operations
    + Logical inversion
  - Binary operations
    + Logical conjunction
    + Logical disjunction
    + Integer comparison
* Control flow
  - if
  - while
  - return
* State storage
  - Untyped variables
* Literal types
  - Integer
  - Boolean 

### Notable exclusions
* Typing
* Short circuit evaluation of boolean statements
* Side effect evaluation
* Floating point arithmetic
* Data structures

## Changelog
### v1.0
* Implement support for basic language features
  - Integer arithmetic
    + Support for integer literals
    + Support for unary operator: - (negation)
    + Support for binary operators implemented: +, -, *, /, and %
  - Boolean logic:
    + Support for boolean literals
    + Support for boolean operators: &&, ||, !
    + Support for boolean comparators: =, !=, >, >=, <, <= 
  - Variable storage and recollection
    + Variables must be declared before initializing
  - if-else statements
  - while loops
  - return statements
  
## Dependencies
* simple-parser.scm
* lex.scm