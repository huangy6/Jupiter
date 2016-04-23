;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016

;; =============================================================================
;;  classes
;; =============================================================================
;;
;; Complete state structure:
;; ((layer) (layer) ((A B) (a-class b-class))))
;;
;; Class structure:
;; ('class parent-class property-layer method-layer) 

(define lookup-class
  (lambda (class-name class-layer)
    (lookup-var class-name (list class-layer))))
      
(define get_class-layer
  (lambda (environment)
    (cond
     ((null? (cdr environment)) (car environment))
     (else (get_class-layer (cdr environment))))))

(define no-parent-class 'null)

(define new-class
  (lambda (parent-class property-layer method-layer)
    (if (eq parent-class no-parent-class)
        (list 'class parent-class property-layer method-layer)
        (list 'class parent-class
              (clayer_merge property-layer (get_property-layer parent-class))
              (clayer_merge method-layer (get_method-layer parent-class))))))

(define get_parent-class cadr)
(define get_property-layer caddr)
(define get_method-layer cadddr)

;; =============================================================================
;;  class layers (clayers)
;; =============================================================================

;; (clayer_merge '((a b) (1 2)) '((c d) (3 4))) => (((a b) c d) ((1 2) 3 4))
(define clayer_merge
  (lambda (layer-a layer-b)
    (list (cons (car layer-a) (car layer-b)) (cons (cadr layer-a) (cadr layer-b)))))

;; returns (default-value indices-remaining)
;; NOTE: indices start at 0!
(define clayer_search
  (lambda (clayer)
    (list
     (layer_lookup-var (list (flatten (car clayer)) (flatten (cadr clayer))))
     (indices-remainig (flatten (car clayer))))))

(define clayer_super-search
  (lambda (layer)
    (if (list? (car layer))
        (clayer_search (cdr layer))
        (error 'clayer_super-search "no super class"))))

(define indices-remaining
  (lambda (l a)
    (cond
      ((null? (cdr l)) (error 'indices-remaining "cound not find variable")) 
      ((eq? a (car l)) (length (cdr l)))
      (else (indices-remaining (cdr l))))))
    
;; =============================================================================
;;  instances
;; =============================================================================

(define new-instance
  (lambda (true-type instance-field-values)
    (list 'instance true-type instance-field-values)))

(define get_instance-type cadr)
(define get_instance-field-values caddr)

(define set_instance-type
  (lambda (instance instance-type)
    (set_at-index instance-type-index instance-type instance)))

(define set_instance-field-values
  (lambda (instance instance-field-values)
    (set_at-index instance-field-values-index instance-field-values instance)))

;; =============================================================================
;;  extras
;; =============================================================================

(define instance-type-index 1)
(define instance-field-values-index 2)

(define flatten
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (append (flatten (car l)) (flatten (cdr l))))
      (else (cons (car l) (flatten (cdr l)))))))

(define set_at-index
    (lambda (index value list)
    (if (zero? index)
        (cons value (cdr list))
        (cons (car list) (set_at-index (- index 1) value (cdr list))))))
