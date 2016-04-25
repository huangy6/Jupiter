;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016

(load "Mstate.scm")

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
    (if (eq? 'null class-name)
        'null
        (lookup-var class-name (list class-layer)))))
      
(define get_class-layer
  (lambda (environment)
    (cond
     ((null? (cdr environment)) (car environment))
     (else (get_class-layer (cdr environment))))))

(define no-parent-class 'null)

(define new-class
  (lambda (parent-class-name parent-class property-layer method-layer)
    (if (eq? parent-class-name no-parent-class)
        (list 'class parent-class-name property-layer method-layer)
            (list 'class parent-class-name 
                  (clayer_merge property-layer (get_property-layer parent-class))
                  (clayer_merge method-layer (get_method-layer parent-class))))))

(define get_parent-class cadr)
(define get_property-layer caddr)
(define get_method-layer cadddr)

;; =============================================================================
;;  class layers (clayers)
;; =============================================================================

;; (clayer_merge '((a b) (1 2)) '((c d) (3 4))) => ((a b c d) (1 2 3 4))
(define clayer_merge
  (lambda (layer-a layer-b)
    (list (append (car layer-a) (car layer-b)) (append (cadr layer-a) (cadr layer-b)))))

;; returns indices remaining
;; NOTE: indices start at 0!
(define clayer_search
  (lambda (var clayer)
    (indices-remaining (flatten (car clayer)) var)))

(define clayer_super-search
  (lambda (var clayer)
    (if (list? (car clayer))
        (clayer_search var (cdr clayer))
        (error 'clayer_super-search "no super class"))))

(define indices-remaining
  (lambda (l a)
    (cond
      ((null? l) (error 'indices-remaining a l "cound not find variable")) 
      ((eq? a (car l)) (length (cdr l)))
      (else (indices-remaining (cdr l) a)))))

(define lookup-method
  (lambda (func-name class state)
      (layer_lookup-var func-name (get_method-layer (lookup-class class (get_class-layer state))))))
    
;; =============================================================================
;;  instances
;; =============================================================================

(define new-instance
  (lambda (true-type state)
    (list 'instance true-type (reverse* (map (lambda (val) (box (unbox val))) (vals (get_property-layer (lookup-class true-type (get_class-layer state)))))))))

(define super-instance
  (lambda (instance c-class state)
    (if (eq? (length (super-properties c-class state))
             (length (get_instance-field-values instance)))
        instance 
        (super-instance (set_instance-field-values instance (cdr (get_instance-field-values instance))) c-class state))))
             

(define super-properties
  (lambda (c-class state)
      (vals (get_property-layer (lookup-class (get_parent-class (lookup-class c-class (get_class-layer state))) (get_class-layer state))))))   
            
(define update-instance-var
  (lambda (var val instance c-class state)
    (set-box! (lookup-instance-var var instance c-class state) val)))
 
(define lookup-instance-var
  (lambda (var instance c-class state)
    (instance-lookup-at-index (flatten (get_instance-field-values instance))
                              (clayer_search var (get_property-layer (lookup-class c-class (get_class-layer state)))))))

(define instance-lookup-at-index
  (lambda (reversed-instance-field-values index)
    (if (zero? index)
        (car reversed-instance-field-values)
        (instance-lookup-at-index (cdr reversed-instance-field-values) (- index 1)))))

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

(define reverse*
  (lambda (l)
    (reverse*-cps l (lambda (v) v))))

(define reverse*-cps
    (lambda (l return)
        (cond
            ((null? l) (return l))
            ((list? (car l)) (reverse*-cps (cdr l) (lambda (v) (reverse*-cps (car l) (lambda (w) (return (append v (cons w '()))))))))
            (else (reverse*-cps (cdr l) (lambda (v) (return (append v (cons (car l) '())))))))))
    

(define set_at-index
    (lambda (index value list)
    (if (zero? index)
        (cons value (cdr list))
        (cons (car list) (set_at-index (- index 1) value (cdr list))))))
