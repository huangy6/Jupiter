;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016

(load "functionParser.scm")
;;(load "simple-parser.scm")

(load "Mvalue.scm")
(load "Mstate.scm")

;(define call/cc call-with-current-continuation)

(define init-state (list init-layer))
(define init-gotos
    (list
        (lambda (v) (error 'goto-error "return goto has not been set"))
        (lambda (v) (error 'goto-error "break goto has not been set"))
        (lambda (v) (error 'goto-error "continue goto has not been set"))
        (lambda (v) (error 'goto-error "throw goto has not been set"))))

(define main-invocation '(funcall main))

(define interpret
  (lambda (filename)
    ((lambda (state)
      ((lookup-var 'main state) '() state))
    (Mstate (parser filename) init-state init-gotos))))

