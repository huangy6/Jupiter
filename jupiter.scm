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
            ((var-declaration-stmt? (car parse-tree)) 'unimplemented)
            ((assigment-stmt? (car parse-tree)) 'unimplemented)
            ((if-stmt? (car parse-tree)) 'unimplemented)
            ((while-stmt? (car parse-tree)) 'unimplemented)
            ((return-stmt? (car parse-tree)) 'unimplemented)
            (else (error 'interpret-parse-tree "unrecognized branch in parse tree")))))
