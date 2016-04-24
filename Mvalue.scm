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
	    ((literal? expression) (literal-eval expression state c-class c-instance))
            ; function call
            ((funcall-expression? expression) (func-eval (operand1 expression) (func-params expression) state c-class c-instance))
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
            ((new-stmt? expression) (new-instance (operand1 expression) state))
            ; dot operator
            ((dot-stmt? expression) (Mvalue_dot (operand1 expression) (operand2 expression) state c-class c-instance))
            (else (error 'unknown "unkown expression")))))

(define instantiate
  (lambda (true-type state)
    (new-instance true-type state)))

(define literal-eval
  (lambda (literal state c-class c-instance)
    (cond
     ((number? literal) literal)
     ((eq? 'true literal) #t)
     ((eq? 'false literal) #f)
     ((var-name? literal)
      (if (layer_contains-var? literal (car state))
          (lookup-var literal state)
          (unbox (lookup-instance-var literal c-instance c-class state)))))))

(define var-name? (lambda (name) #t))

(define func-eval
  (lambda (func-expression args state c-class c-instance)
    ; check for the dot operator
    (if (list? func-expression)
        ; call the dot operator
        ((lookup-method (caddr func-expression) (car (Mobject (cadr func-expression) state c-class c-instance)) state)
         (map (lambda (arg) (Mvalue_expression arg state c-class c-instance)) args)
         state
         ;(car (Mobject (cadr func-expression) state c-class c-instance))
         (cadr (Mobject (cadr func-expression) state c-class c-instance)))
        ; otherwise look up in the current class (SHOULD BE INSTANCE'S CLASS?)
        ((lookup-method func-expression (get_instance-type c-instance) state) (map (lambda (arg) (Mvalue_expression arg state c-class c-instance)) args) state c-instance))))

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

;; properties only, funccalls are in ________
(define Mvalue_dot
  (lambda (lhs rhs state c-class c-instance)
    (unbox (lookup-instance-var rhs (cadr (Mobject lhs state c-class c-instance)) (car (Mobject lhs state c-class c-instance)) state))))
    
