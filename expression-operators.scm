;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016

(define neg-operator -)
(define bool_not-operator not)

(define add-operator +)
(define sub-operator -)
(define mult-operator *)
(define div-operator quotient)
(define mod-operator remainder)
(define eq?-operator equal?)
(define noteq?-operator (lambda (a b) (not (eq?-operator a b))))
(define lt?-operator <)
(define gt?-operator >)
(define lteq?-operator <=)
(define gteq?-operator >=)
(define bool_and-operator and)
(define bool_or-operator or)
