;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016

(load "class-parser.scm")

(load "Mvalue.scm")
(load "Mstate.scm")
(load "OOP.scm")

(define init-state (list init-layer))

(define main-invocation '(funcall main))

(define interpret
  (lambda (filename classname)
    (initialize_classes (parser filename) init-state)))

