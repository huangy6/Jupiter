; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(load "simple-parser.scm")
(load "stmt-conds.scm")
(load "mvalue.scm")
(load "mstate.scm")

(define interpret
    (lambda (filename)
        (Mvalue_return (Mstate_replace-bools (Mstate (parser filename) '(()()))))))

(define branch car)
(define first-param cadar)
(define second-param caddar)
(define third-param
    (lambda (tree)
        (cadddr (branch tree))))
