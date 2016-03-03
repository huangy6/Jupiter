;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016
;;
;; state structure is (S1 S2 S3 ...)
;; where each Si has the format '((x y z ...) (3 5 7 ...))

;; =============================================================================
;                                   Core
; =============================================================================
(load "stmt-conds.scm")

(define branch car)
(define first-param cadr)
(define second-param caddr)
(define second-param? (lambda (l) (not (null? (cddr l)))))
(define third-param cadddr)
(define third-param? (lambda (l) (not (null? (cdddr l)))))

(define Mstate
    (lambda (parse-tree state)
      (cond
            ((null? parse-tree) state)
            ((var-declaration-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_var-declaration-stmt (first-param (branch parse-tree)) (if (second-param? (branch parse-tree)) (second-param (branch parse-tree)) (list)) state)))
            ((assigment-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_assignment-stmt (first-param (branch parse-tree)) (Mvalue_expression (second-param (branch parse-tree)) state) state)))
            ((if-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_if-else-stmt (first-param (branch parse-tree)) (second-param (branch parse-tree)) (if (third-param? (branch parse-tree)) (third-param (branch parse-tree)) (list)) state)))
            ((while-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_while-stmt (first-param (branch parse-tree)) (second-param (branch parse-tree)) state)))
            ((return-stmt? (branch parse-tree)) (Mstate_return-stmt (first-param (branch parse-tree)) state))
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
    (Mstate_update-var variable (if (null? expression)
				    expression
				    (Mvalue_expression expression state))
		       (Mstate_insert-var variable state))))

; if else
(define Mstate_if-else-stmt
    (lambda (condition then-stmt else-stmt state)
        (if (Mvalue_expression condition state)
            (Mstate (list then-stmt) state)
            (if (null? else-stmt)
		state
		(Mstate (list else-stmt) state)))))

; while
(define Mstate_while-stmt
  (lambda (condition do-stmt state)
    (if (Mvalue_expression condition state)
	   (Mstate_while-stmt condition do-stmt (Mstate (list do-stmt) state))
       state)))


;; =============================================================================
;;  "gotos" abstractions
;; =============================================================================

(define return-goto car)
(define break-goto cadr)
(define continue-goto caddr)
(define throw-goto cadddr)

(define gotos/new-return
  (lambda (new-return gotos)
    (gotos/new 'return new-return gotos)))

(define gotos/new-break
  (lambda (new-return gotos)
    (gotos/new 'break new-return gotos)))

(define gotos/new-continue
  (lambda (new-return gotos)
    (gotos/new 'continue new-return gotos)))

(define gotos/new-throw
  (lambda (new-return gotos)
    (gotos/new 'throw new-return gotos)))

(define gotos/new
  (lambda (goto-name new-goto gotos)
    (cond
      ((eq? goto-name 'return)   (update-at-index 0 new-goto gotos))
      ((eq? goto-name 'break)    (update-at-index 1 new-goto gotos))
      ((eq? goto-name 'continue) (update-at-index 2 new-goto gotos))
      ((eq? goto-name 'throw)    (update-at-index 3 new-goto gotos)))))

(define update-at-index
  (lambda (index new-element list)
    (if (zero? index)
        (cons new-element (cdr list))
        (update-at-index ((- index 1) new-element (cdr list))))))

;; =============================================================================
;;                                 Helpers
;; =============================================================================

(define vars car)
(define vals cadr)
(define init_var_state list)
(define init-layer (list (list) (list)))
(define current-layer car)
(define Mstate_shed-layer cdr)

;; Appends a new working layer to the front of the state
(define Mstate_add-layer (lambda (layer state) (cons layer state)))

; replaces occurences of #t with 'true and #f with 'false
(define Mstate_replace-bools
    (lambda (state)
        (Mstate_construct (vars state) (replace-bools-in-values (vals state)))))

(define replace-bools-in-values
    (lambda (values)
        (cond
            ((null? values) '())
            ((eq? #t (car values)) (cons 'true (replace-bools-in-values (cdr values))))
            ((eq? #f (car values)) (cons 'false (replace-bools-in-values (cdr values))))
            (else (cons (car values) (replace-bools-in-values (cdr values)))))))


;; =============================================================================
;;  state functions - PUBLIC
;; =============================================================================

;; takes a list of variables and a list of values and returns a state layer
;; according to the structure defined at the top of this file
(define Mstate_construct
    (lambda (variables values)
        (append (list variables) (list values))))

;; takes a variable, a value, and a state and updates the value of the
;; variable, returning the state; produces an error if variable not declared
(define Mstate_update-var
  (lambda (variable value state)
    (cond
     ((null? state) (error 'Mstate_update-var "Variable has not been declared"))
     ((layer_contains-var? variable (current-layer state)) (Mstate_add-layer (layer_update-var variable value (current-layer state)) (Mstate_shed-layer state)))
     (else (Mstate_add-layer (current-layer state) (Mstate_update-var variable value (Mstate_shed-layer state)))))))

 ;; takes a variable and a state and returns the value of that variable
 ;; if it exists and is not null, otherwise produces an error
 (define lookup-var
   (lambda (variable state)
     (cond
      ((null? state) (error 'lookup-var "variable name not found"))
      ((layer_contains-var? variable (current-layer state)) (layer_lookup-var variable state))
      (else (lookup-var variable (Mstate_shed-layer state))))))

;; takes a variable and state and returns the state with the vairable
;; initialized to the empty list
(define Mstate_insert-var
    (lambda (variable state)
        (Mstate_construct (cons variable (vars state))
                          (cons (init_var_state) (vals state)))))

  ;; takes a variable and state and returns true if variable is a member
  ;; of the state, otherwise returns false
  (define contains-var?
    (lambda (variable state)
      ((null? state) #f)
      ((layer_contains-var? variable (current-layer state)) #t)
      (else (contains-var? variable (Mstate_shed-layer state)))))

;; takes two states and merges (NOT union) them together
(define Mstate_merge
    (lambda (left-state right-state)
        (Mstate_construct
            (append (vars left-state) (vars right-state))
            (append (vals left-state) (vals right-state)))))

;; =============================================================================
;;  layer functions
;; =============================================================================

;; wraps a list of variables and list of values into a new "layer"
(define layer_construct
  (lambda (variables values)
    (list variables values)))

;; takes a variable and a state layer and updates the value for the provided
;; variable
(define layer_update-var
  (lambda (variable value layer)
    (cond
     ((null? layer) (error 'layer_lookup-var "variable name not found in layer")) ; Shouldn't be called
     ((eq? variable (car (vars layer))) (layer_construct (vars layer) (cons value (cdr (vals state)))))
     (else (layer_add-binding (car (vars state)) (car (vals state)) (layer_update-var variable value (cdr layer)))))))

;; takes a variable and a state layer and returns the value of the variable
;; unless it is null.
(define layer_lookup-var
  (lambda (variable layer)
    (cond
     ((null? layer) (error 'layer_lookup-var "variable name not found in layer")) ; Shouldn't get called either
     ((eq? variable (car (vars layer))) (if (null? (car (vals state)))
					    (error 'layer_lookup-var "variable null")
					    (car (vals state))))
     (else (layer_lookup-var variable (layer_construct (cdr (vars state)) (cdr (vals state))))))))

 ;; takes a variable and layer and returns true if variable is a member
 ;; of the layer, otherwise returns false
 (define layer_contains-var?
   (lambda (variable layer)
     (member variable (vars layer))))

;; adds a new binding onto the current layer, UNSAFE
 (define layer_add-binding
   (lambda (variable value layer)
     (layer_construct (cons variable (vars layer)) (cons value (vals layer)))))
