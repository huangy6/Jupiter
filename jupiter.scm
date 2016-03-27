; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(load "function-parser.scm")
(load "Mvalue.scm")
(load "Mstate.scm")

(define init-state (list init-layer))
(define call/cc call-with-current-continuation)

(define interpret
    (lambda (filename)
                (Mvalue_return (Mstate_replace-bools
                    (call/cc
                        (lambda (return)
                            (Mstate (parser filename) init-state (gotos/new-return return init-gotos))))))))
