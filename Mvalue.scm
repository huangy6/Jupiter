; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(define Mvalue_expression
    (lambda (expression state)
        (cond
            ((number? expression) expression)
            ((eq? 'true expression) #t)
            ((eq? 'false expression) #f)
            ; a variable
            ((not (pair? expression)) (Mstate_lookup-var expression state))
            ; mathematical operators
            ((eq? (operator expression) '+) (+ (Mvalue_expression (left-operand expression) state) (Mvalue_expression (right-operand expression) state)))
            ((and (eq? (operator expression) '-) (null? (cddr expression))) (- (Mvalue_expression (left-operand expression) state)))
            ((eq? (operator expression) '-) (- (Mvalue_expression (left-operand expression) state) (Mvalue_expression (right-operand expression) state)))
            ((eq? (operator expression) '*) (* (Mvalue_expression (left-operand expression) state) (Mvalue_expression (right-operand expression) state)))
            ((eq? (operator expression) '/) (quotient (Mvalue_expression (left-operand expression) state) (Mvalue_expression (right-operand expression) state)))
            ((eq? (operator expression) '%) (remainder (Mvalue_expression (left-operand expression) state) (Mvalue_expression (right-operand expression) state)))
            ; comparision operators
            ((eq? (operator expression) '==) (equal? (Mvalue_expression (left-operand expression) state) (Mvalue_expression (right-operand expression) state)))
            ((eq? (operator expression) '!=) (not (equal? (Mvalue_expression (left-operand expression) state) (Mvalue_expression (right-operand expression) state))))
            ((eq? (operator expression) '<) (< (Mvalue_expression (left-operand expression) state) (Mvalue_expression (right-operand expression) state)))
            ((eq? (operator expression) '>) (> (Mvalue_expression (left-operand expression) state) (Mvalue_expression (right-operand expression) state)))
            ((eq? (operator expression) '<=) (<= (Mvalue_expression (left-operand expression) state) (Mvalue_expression (right-operand expression) state)))
            ((eq? (operator expression) '>=) (>= (Mvalue_expression (left-operand expression) state) (Mvalue_expression (right-operand expression) state)))
            ; boolean operators
            ((eq? (operator expression) '&&) (and (Mvalue_expression (left-operand expression)) (Mvalue_expression (right-operand expression) state)))
            ((eq? (operator expression) '||) (or (Mvalue_expression (left-operand expression)) (Mvalue_expression (right-operand expression) state)))
            ((eq? (operator expression) '!) (not (Mvalue_expression expression state)))

            (else (error 'unknown "unkown expression")))))

(define Mvalue_return-stmt
    (lambda (state)
        (Mstate_lookup-var 'return state)))

(define operator car)
(define left-operand cadr)
(define right-operand caddr)
