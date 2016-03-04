; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(load "simple-parser.scm")
(load "Mvalue.scm")
(load "Mstate.scm")

(define init-state (list init-layer))

(define interpret
    (lambda (filename)
        (Mvalue_return (Mstate_replace-bools (Mstate (parser filename) init-state init-gotos)))))
