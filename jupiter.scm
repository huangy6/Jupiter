; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(load "simple-parser.scm")
(load "stmt-conds.scm")

(define interpret
    (lambda (filename)
        (interpret-parse-tree (parser filename) '(()()))))

(define interpret-parse-tree
    (lambda (parse-tree state)
        (cond
            ((var-declaration-stmt? (branch parse-tree)) 'unimplemented)
            ((assigment-stmt? (branch parse-tree)) 'unimplemented)
            ((if-stmt? (branch parse-tree)) 'unimplemented)
            ((while-stmt? (branch parse-tree)) 'unimplemented)
            ((return-stmt? (branch parse-tree)) 'unimplemented)
            (else (error 'interpret-parse-tree "unrecognized branch in parse tree")))))

(define branch car)
(define first-param cadar)
(define second-param caddar)
(define third-param
    (lambda (tree)
        (cadddr (branch tree))))
