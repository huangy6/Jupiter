; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

; state structure is '((x y z ...) (3 5 7 ...))

; =============================================================================
;                                   Core
; =============================================================================
(load "Mvalue.scm")
(load "stmt-conds.scm")

(define branch car)
(define first-param cadar)
(define second-param caddar)
(define third-param cadddar)

(define Mstate
    (lambda (parse-tree state)
        (cond
            ((var-declaration-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_var-declaration-stmt (first-param parse-tree) (second-param parse-tree) state))))
            ((assigment-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_assignment-stmt (first-param parse-tree) (second-param parse-tree) state)))
            ((if-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_if-else-stmt (first-param parse-tree) (second-param parse-tree) (third-param parse-tree) state))))
            ((while-stmt? (branch parse-tree)) 'unimplemented) ; TODO
            ((return-stmt? (branch parse-tree)) (Mstate_return-stmt (first-param parse-tree) state))
            (else (error 'interpret-parse-tree "unrecognized branch in parse tree")))))

; insert the 'return var into the state
(define Mstate_return-stmt
    (lambda (return-stmt state)
        (Mstate_var-declaration-stmt 'return return-stmt state)))

; assigment
(define Mstate_assignment-stmt
    (lambda (variable value state)
        (Mstate_update-var variable value state)))

; declaration
(define Mstate_var-declaration-stmt
  (lambda (variable expression state)
    (Mstate_update-var variable ((lambda (exp) (if (null? exp)
 						   expression
						   (Mvalue_expression expression (Mstate_insert-var variable state)))) expression) (Mstate_insert-var variable state))))

; if else
(define Mstate_if-else-stmt
    (lambda (condition then-stmt else-stmt state)
        (if (Mvalue_expression condition state)
            (Mstate_assignment-stmt (cadr then-stmt) (Mvalue_expression (caddr then-stmt) state) state)
            ((lambda (else) (if (null? else)
		   	        state
			        (Mstate_assignment-stmt (cadr else-stmt) (Mvalue_expression (caddr else-stmt) state)))) else-stmt))))


;; =============================================================================
;;                                 Helpers
;; =============================================================================

(define Mstate_variables car)
(define Mstate_values cadr)
(define init_var_state list)

;; takes a list of variables and a list of values and returns the state
;; according to the structure defined at the top of this file
(define Mstate_construct
    (lambda (variables values)
        (append (list variables) (list values))))

;; takes a variable and state and returns the state with the vairable
;; initialized to the empty list
(define Mstate_insert-var
    (lambda (variable state)
        (Mstate_construct (cons variable (Mstate_variables state))
                          (cons (init_var_state) (Mstate_values state)))))

;; takes a variable and state and returns true if variable is a member
;; of the state, otherwise returns false
(define contains-var?
    (lambda (variable state)
        (member variable (Mstate_variables state))))

; takes two states and merges (NOT union) them together
(define Mstate_merge
    (lambda (left-state right-state)
        (Mstate_construct
            (append (Mstate_variables left-state) (Mstate_variables right-state))
            (append (Mstate_values left-state) (Mstate_values right-state)))))

;; takes a variable, a value, and a state and updates the value of the
;; variable, returning the state; produces an error if variable not declared
(define Mstate_update-var
    (lambda (variable value state)
        (if (contains-var? variable state)
            (cond
                ((null? state) '())
                ((eq? variable (car (Mstate_variables state))) (Mstate_construct (Mstate_variables state) (cons value (cdr (Mstate_values state)))))
                (else (Mstate_merge (Mstate_construct (list (car (Mstate_variables state))) (list (car (Mstate_values state))))
                                    (Mstate_update-var variable value (Mstate_construct (cdr (Mstate_variables state)) (cdr (Mstate_values state)))))))
            (error 'Mstate_update-var "variable has not been declared"))))


;; takes a variable and a state and returns the value of that variable
;; if it exists and is not null, otherwise produces an error
(define lookup-var
    (lambda (variable state)
        (if (contains-var? variable state)
            (cond
                ((eq? variable (car (Mstate_variables state)))
                    (if (null? (car (Mstate_values state)))
                        (error 'lookup-var "variable has not been assigned a value")
                        (car (Mstate_values state))))
                (else (lookup-var variable (Mstate_construct (cdr (Mstate_variables state)) (cdr (Mstate_values state))))))
            (error 'lookup-var "variable has not been declared"))))
