;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016

(load "expression-conds.scm")
(load "expression-operators.scm")

(define literal? (lambda (exp) (not (pair? exp))))
(define operand1 cadr)
(define operand2 caddr)

(define Mvalue_expression
    (lambda (expression state)
        (cond
            ; literal
	    ((literal? expression) (literal-eval expression state))
            ; mathematical operators
            ((math_neg-expression? expression) (neg-operator (Mvalue_expression (operand1 expression) state)))
            ((add-expression? expression) (add-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ((sub-expression? expression) (sub-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ((mult-expression? expression) (mult-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ((div-expression? expression) (div-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ((mod-expression? expression) (mod-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ; comparision operators
            ((eq?-expression? expression) (eq?-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ((noteq?-expression? expression) (noteq?-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ((lt?-expression? expression) (lt?-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ((gt?-expression? expression) (gt?-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ((lteq?-expression? expression) (lteq?-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ((gteq?-expression? expression) (gteq?-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ; boolean operators
            ((bool_and-expression? expression) (bool_and-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ((bool_or-expression? expression) (bool_or-operator (Mvalue_expression (operand1 expression) state) (Mvalue_expression (operand2 expression) state)))
            ((bool_neg-expression? expression) (bool_not-operator (Mvalue_expression (operand1 expression) state)))
            (else (error 'unknown "unkown expression")))))

(define literal-eval
  (lambda (literal state)
    (cond
     ((number? literal) literal)
     ((eq? 'true literal) #t)
     ((eq? 'false literal) #f)
     ((var-name? literal) (lookup-var literal state)))))

(define var-name? (lambda (name) #t))

(define Mvalue_return
    (lambda (state)
        (lookup-var 'return state)))
