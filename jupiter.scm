; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(load "simple-parser.scm")

(define interpret
    (lambda (filename)
        (interpret-parse-tree (parser filename))))
