;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016

(load "expression-conds.scm")
(load "expression-operators.scm")

(define literal? (lambda (exp) (not (pair? exp))))
(define operand1 cadr)
(define operand2 caddr)
(define func-params cddr)

(define Mvalue_expression
    (lambda (expression state c-class c-instance gotos)
        (cond
            ;; literal
	    ((literal? expression) (literal-eval expression state c-class c-instance))
            ;; function call
            ((funcall-expression? expression) (func-eval (operand1 expression) (func-params expression) state c-class c-instance gotos))
            ;; mathematical operators
            ((math_neg-expression? expression) (neg-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos)))
            ((add-expression? expression) (add-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ((sub-expression? expression) (sub-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ((mult-expression? expression) (mult-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ((div-expression? expression) (div-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ((mod-expression? expression) (mod-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ;; comparision operators
            ((eq?-expression? expression) (eq?-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ((noteq?-expression? expression) (noteq?-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ((lt?-expression? expression) (lt?-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ((gt?-expression? expression) (gt?-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ((lteq?-expression? expression) (lteq?-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ((gteq?-expression? expression) (gteq?-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ;; boolean operators
            ((bool_and-expression? expression) (bool_and-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ((bool_or-expression? expression) (bool_or-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos) (Mvalue_expression (operand2 expression) state c-class c-instance gotos)))
            ((bool_neg-expression? expression) (bool_not-operator (Mvalue_expression (operand1 expression) state c-class c-instance gotos)))
            ;; instances
            ((new-stmt? expression) (new-instance (operand1 expression) state))
            ;; dot operator
            ((dot-stmt? expression) (Mvalue_dot (operand1 expression) (operand2 expression) state c-class c-instance gotos))
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
     ((eq? 'this literal) c-instance)
     ((var-name? literal)
      (if (contains-var? literal state)
          (lookup-var literal state)
          (unbox (lookup-instance-var literal c-instance c-class state)))))))

(define var-name? (lambda (name) #t))

(define func-eval
  (lambda (func-expression args state c-class c-instance gotos)
    ;; check for the dot operator
    (cond
     ((dotted? func-expression) ((lookup-method (caddr func-expression) (car (Mobject (cadr func-expression) state c-class c-instance gotos)) state)
				 (map (lambda (arg) (Mvalue_expression arg state c-class c-instance gotos)) args)
				 (shed-necessary-layers state)
				 gotos
				 (cadr (Mobject (cadr func-expression) state c-class c-instance gotos))))
     ;; otherwise look up in the current class (SHOULD BE INSTANCE'S CLASS?)
     ((contains-var? func-expression state) ((lookup-var func-expression state)
					     (map (lambda (arg) (Mvalue_expression arg state c-class c-instance gotos)) args)
					     state
					     (gotos/new-throw (lambda (e func-env) ((throw-goto gotos) e state)) gotos)
					     c-instance))
     (else ((lookup-method func-expression (get_instance-type c-instance) state)
	    (map (lambda (arg) (Mvalue_expression arg state c-class c-instance gotos)) args)
	    (shed-necessary-layers state)
	    (gotos/new-throw (lambda (e func-env) ((throw-goto gotos) e state)) gotos)
	    c-instance)))))

(define shed-necessary-layers
  (lambda (state)
    (list (get_class-layer state))))

(define Mvalue_return
    (lambda (state)
      (if (contains-var? 'return state)
        (lookup-var 'return state)
        'void)))

;; properties only, funccalls are in ________
(define Mvalue_dot
  (lambda (lhs rhs state c-class c-instance gotos)
    (unbox (lookup-instance-var rhs (cadr (Mobject lhs state c-class c-instance gotos)) (car (Mobject lhs state c-class c-instance gotos)) state))))
    
