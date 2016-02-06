; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(load "simple-parser.scm")

(define interpret
    (lambda (filename)
        (interpret-parse-tree (parser filename) '(()()))))

(define interpret-parse-tree
    (lambda (parse-tree state) (error 'missing "interpret-parse-tree not implemented"))
