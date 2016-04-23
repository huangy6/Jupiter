;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016

(load "expression-conds.scm")
(load "expression-operators.scm")

(define literal? (lambda (exp) (not (pair? exp))))
(define operand1 cadr)
(define operand2 caddr)
(define func-params cddr)

(define Mvalue_expression
    (lambda (expression state c-class c-instance)
        (cond
            ; literal
	    ((literal? expression) (literal-eval expression state))
            ; function call
            ((funcall-expression? expression) (func-eval (operand1 expression) (func-params expression) state c-class c-instance) c-class c-instance)
            ; mathematical operators
            ((math_neg-expression? expression) (neg-operator (Mvalue_expression (operand1 expression) state c-class c-instance)))
            ((add-expression? expression) (add-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ((sub-expression? expression) (sub-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ((mult-expression? expression) (mult-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ((div-expression? expression) (div-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ((mod-expression? expression) (mod-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ; comparision operators
            ((eq?-expression? expression) (eq?-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ((noteq?-expression? expression) (noteq?-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ((lt?-expression? expression) (lt?-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ((gt?-expression? expression) (gt?-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ((lteq?-expression? expression) (lteq?-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ((gteq?-expression? expression) (gteq?-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ; boolean operators
            ((bool_and-expression? expression) (bool_and-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ((bool_or-expression? expression) (bool_or-operator (Mvalue_expression (operand1 expression) state c-class c-instance) (Mvalue_expression (operand2 expression) state c-class c-instance)))
            ((bool_neg-expression? expression) (bool_not-operator (Mvalue_expression (operand1 expression) state c-class c-instance)))
            ; instances
            ((new-stmt? expression) (new-instance (operand1 expression) state c-class c-instance))     
            (else (error 'unknown "unkown expression")))))

(define instantiate
  (lambda (true-type state)
    (new-instance true-type state)))

(define literal-eval
  (lambda (literal state)
    (cond
     ((number? literal) literal)
     ((eq? 'true literal) #t)
     ((eq? 'false literal) #f)
     ((var-name? literal) (lookup-var literal state)))))

(define var-name? (lambda (name) #t))

(define func-eval
  (lambda (func-name args state c-class c-instance)
    (begin
      ;(display "\n\n")
      ;(display func-name)
      ;(display "\n")
      ;(display (shed-necessary-layers func-name state))
      
    ((lookup-var func-name state) (map (lambda (arg) (Mvalue_expression arg state c-class c-instance)) args) (shed-necessary-layers func-name state)))))

(define shed-necessary-layers
  (lambda (func-name state)
    (cond
      ((null? state) (error 'shed-necessary-layers "state is empty"))
      ((layer_contains-var? func-name (current-layer state)) state)
      (else (shed-necessary-layers func-name (Mstate_shed-layer state))))))

(define Mvalue_return
    (lambda (state)
      (if (contains-var? 'return state)
        (lookup-var 'return state)
        'void)))
