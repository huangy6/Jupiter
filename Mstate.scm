; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

; state structure is '((x y z ...) (3 5 7 ...))

; =============================================================================
;                                   Core
; =============================================================================

(define Mstate
    (lambda (parse-tree state)
        (cond
            ((var-declaration-stmt? (branch parse-tree))
                (if (null? (cddar parse-tree))
                    (Mstate (cdr parse-tree) (Mstate_var-declaration-stmt (first-param parse-tree) state))
                    (Mstate (cdr parse-tree) (Mstate_var-declaration-stmt-with-value (first-param parse-tree) (second-param parse-tree) state))))
            ((assigment-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_assignment-stmt (first-param parse-tree) (second-param parse-tree) state)))
            ((if-stmt? (branch parse-tree))
                (if (null? (cdddar parse-tree))
                    (Mstate (cdr parse-tree) (Mstate_if-stmt (first-param parse-tree) (second-param parse-tree) state))
                    (Mstate (cdr parse-tree) (Mstate_if-else-stmt (first-param parse-tree) (second-param parse-tree) (third-param parse-tree) state))))
            ((while-stmt? (branch parse-tree)) 'unimplemented)
            ((return-stmt? (branch parse-tree)) (Mstate_return-stmt (first-param parse-tree) state))
            (else (error 'interpret-parse-tree "unrecognized branch in parse tree")))))

; insert the 'return var into the state
(define Mstate_return-stmt
    (lambda (return-stmt state)
        (Mstate_var-declaration-stmt-with-value 'return return-stmt state)))

; assigment
(define Mstate_assignment-stmt
    (lambda (variable value state)
        (Mstate_update-var variable value state)))

; declaration
(define Mstate_var-declaration-stmt
    (lambda (variable state)
        (Mstate_update-var variable '() (Mstate_insert-var variable state))))

(define Mstate_var-declaration-stmt-with-value
    (lambda (variable value state)
        (Mstate_update-var variable (Mvalue_expression value (Mstate_insert-var variable state)) (Mstate_insert-var variable state))))

; if | if else
(define Mstate_if-stmt
    (lambda (condition stmt state)
        (if (Mvalue_expression condition state)
            (Mstate_assignment-stmt (cadr stmt) (Mvalue_expression (caddr stmt) state) state)
            state)))

(define Mstate_if-else-stmt
    (lambda (condition stmt1 stmt2 state)
        (if (Mvalue_expression condition state)
            (Mstate_assignment-stmt (cadr stmt1) (Mvalue_expression (caddr stmt1) state) state)
            (Mstate_assignment-stmt (cadr stmt2) (Mvalue_expression (caddr stmt2) state) state))))


; =============================================================================
;                                 Helpers
; =============================================================================

(define Mstate_variables car)
(define Mstate_values cadr)

; takes a list of variables and a list of values and returns the state
; according to the structure defined at the top of this file
(define Mstate_construct
    (lambda (variables values)
        (append (cons variables '()) (cons values '()))))

; takes a variable and state and returns the state with the vairable
; initialized to the empty list
(define Mstate_insert-var
    (lambda (variable state)
        (Mstate_construct (cons variable (Mstate_variables state))
                          (cons '() (Mstate_values state)))))

; takes a variable and state and returns true if variable is a member
; of the state, otherwise returns false
(define Mstate_contains-var?
    (lambda (variable state)
        (member variable (Mstate_variables state))))

; takes two states and merges (NOT union) them together
(define Mstate_merge
    (lambda (left-state right-state)
        (Mstate_construct
            (append (Mstate_variables left-state) (Mstate_variables right-state))
            (append (Mstate_values left-state) (Mstate_values right-state)))))

; takes a variable, a value, and a state and updates the value of the
; variable, otherwise produces an error
(define Mstate_update-var
    (lambda (variable value state)
        (if (Mstate_contains-var? variable state)
            (cond
                ((null? state) '())
                ((eq? variable (car (Mstate_variables state))) (Mstate_construct (Mstate_variables state) (cons value (cdr (Mstate_values state)))))
                (else (Mstate_merge (Mstate_construct (cons (car (Mstate_variables state)) '()) (cons (car (Mstate_values state)) '()))
                                    (Mstate_update-var variable value (Mstate_construct (cdr (Mstate_variables state)) (cdr (Mstate_values state)))))))
            (error 'Mstate_update-var "variable has not been declared"))))


; takes a variable and a state and returns the value of that variable
; if it exists and is not null, otherwise produces an error
(define Mstate_lookup-var
    (lambda (variable state)
        (if (Mstate_contains-var? variable state)
            (cond
                ((eq? variable (car (Mstate_variables state)))
                    (if (null? (car (Mstate_values state)))
                        (error 'Mstate_lookup-var "variable has not been assigned a value")
                        (car (Mstate_values state))))
                (else (Mstate_lookup-var variable (Mstate_construct (cdr (Mstate_variables state)) (cdr (Mstate_values state))))))
            (error 'Mstate_lookup-var "variable has not been declared"))))
