; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(load "simple-parser.scm")
(load "stmt-conds.scm")
(load "mvalue.scm")
(load "mstate.scm")

(define init-state (list (list) (list)))

(define interpret
    (lambda (filename)
        (Mvalue_return (Mstate (parser filename) init-state))))
