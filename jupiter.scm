;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016

(load "functionParser.scm")
;;(load "simple-parser.scm")

(load "Mvalue.scm")
(load "Mstate.scm")

;(define call/cc call-with-current-continuation)

(define init-state (list init-layer))

(define main-invocation '(funcall main))

(define interpret
  (lambda (filename)
    ((lambda (state)
      ((lookup-var 'main state) '() state))
    (Mstate (parser filename) init-state init-gotos))))

