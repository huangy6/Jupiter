;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016

(require racket/trace)

(load "class-parser.scm")

(load "Mvalue.scm")
(load "Mstate.scm")
(load "OOP.scm")

(define init-state (list init-layer))

(define main-invocation '(funcall main))
(define no-actual-params '())

(define interpret
  (lambda (filename classname)
    ((lambda (state)
       ((lookup-var 'main (list (get_method-layer (lookup-class (string->symbol classname) (get_class-layer state))))) no-actual-params state (lookup-class (string->symbol classname) (get_class-layer state)) 'NO_INSTANCE))
       (initialize_classes (parser filename) init-state))))

