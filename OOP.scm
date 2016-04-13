;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016

;; =============================================================================
;;  classes
;; =============================================================================
(define no-parent-class 'null)

(define new-class
  (lambda (parent-class class-name)
    (if (eq parent-class no-parent-class)
        (list 'class class-name parent-class '() '())
        (list 'class class-name parent-class (get_instance-fields parent-class) (get_methods parent-class)))))

(define get_class-name cadr)
(define get_parent-class caddr)
(define get_instance-fields cadddr)
(define get_methods
  (lambda (list)
    (car (cddddr))))

(define set_instance-fields
  (lambda (class instance-fields)
    (set_at-index 3 instance-fields class)))

(define set_methods
  (lambda (class methods)
    (set_at-index 4 methods class)))

;; =============================================================================
;;  instances
;; =============================================================================

(define new-instance
  (lambda (true-type instance-field-values)
    (list 'instance true-type instance-field-values)))

(define get_true-type cadr)
(define get_instance-field-values caddr)

(define set_instance-field-values
  (lambda (instance instance-field-values)
    (set_at-index 2 instance-field-values instance)))

;; =============================================================================
;;  extras
;; =============================================================================

(define set_at-index
    (lambda (index value list)
    (if (zero? index)
        (cons value (cdr list))
        (cons (car list) (set_at-index (- index 1) value (cdr list))))))