; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(load "simple-parser.scm")
(load "stmt-conds.scm")
(load "mvalue.scm")
(load "mstate.scm")

(define interpret
    (lambda (filename)
        (interpret-parse-tree (parser filename) '(()()))))

(define interpret-parse-tree
    (lambda (parse-tree state)
        (cond
            ((var-declaration-stmt? (branch parse-tree))
                (if (null? (cddar parse-tree))
                    (interpret-parse-tree (cdr parse-tree) (Mstate_var-declaration-stmt (first-param parse-tree) state))
                    (interpret-parse-tree (cdr parse-tree) (Mstate_var-declaration-stmt-with-value (first-param parse-tree) (second-param parse-tree) state))))
            ((assigment-stmt? (branch parse-tree)) (interpret-parse-tree (cdr parse-tree) (Mstate_assignment-stmt (first-param parse-tree) (second-param parse-tree) state)))
            ((if-stmt? (branch parse-tree))
                (if (null? (cdddar parse-tree))
                    (interpret-parse-tree (cdr parse-tree) (Mstate_if-stmt (first-param parse-tree) (second-param parse-tree) state))
                    (interpret-parse-tree (cdr parse-tree) (Mstate_if-else-stmt (first-param parse-tree) (second-param parse-tree) (third-param parse-tree) state))))
            ((while-stmt? (branch parse-tree)) 'unimplemented)
            ((return-stmt? (branch parse-tree)) (Mvalue_expression (first-param parse-tree) state))
            (else (error 'interpret-parse-tree "unrecognized branch in parse tree")))))

(define branch car)
(define first-param cadar)
(define second-param caddar)
(define third-param
    (lambda (tree)
        (cadddr (branch tree))))
