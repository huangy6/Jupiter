;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016
;;
;; state structure is (S1 S2 S3 ...)
;; where each Si has the format '((x y z ...) (3 5 7 ...))

;; =============================================================================
;;                                   Core
;; =============================================================================
(load "stmt-conds.scm")

(define branch car)
(define first-param cadr)
(define second-param caddr)
(define second-param? (lambda (l) (not (null? (cddr l)))))
(define third-param cadddr)
(define third-param? (lambda (l) (not (null? (cdddr l)))))
(define null-param (list))

(define Mstate
  (lambda (parse-tree state gotos)
    (cond
     ((null? parse-tree) state)
     ((var-declaration-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_var-declaration-stmt (first-param (branch parse-tree)) (if (second-param? (branch parse-tree))
																	      (second-param (branch parse-tree))
																	      null-param)
													state gotos) gotos))
     ((assigment-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_assignment-stmt (first-param (branch parse-tree)) (Mvalue_expression (second-param (branch parse-tree)) state gotos) state gotos) gotos))
     ((if-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_if-else-stmt (first-param (branch parse-tree)) (second-param (branch parse-tree)) (if (third-param? (branch parse-tree))
																			    (third-param (branch parse-tree))
																			    null-param)
										   state gotos) gotos))
     ((while-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_while-stmt (first-param (branch parse-tree)) (second-param (branch parse-tree)) state gotos) gotos))
     ((stmt-block? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_stmt-block (cdr (branch parse-tree)) (Mstate_push-layer init-layer state) gotos) gotos))
     ((break-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_break state gotos) gotos))
     ((continue-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_continue state gotos) gotos))
     ((try-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_try-stmt (first-param (branch parse-tree))
										(second-param (branch parse-tree))
										(third-param (branch parse-tree))
										(Mstate_push-layer init-layer state)
										gotos) gotos))
     ((throw-stmt? (branch parse-tree)) (Mstate_throw (first-param (branch parse-tree)) state gotos))
     ((return-stmt? (branch parse-tree)) (Mstate_return-stmt (first-param (branch parse-tree)) state gotos))
     ((func-def-stmt? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_func-def (first-param (branch parse-tree)) (second-param (branch parse-tree)) (third-param (branch parse-tree)) state gotos) gotos))
     ((funcall? (branch parse-tree)) (Mstate (cdr parse-tree) (Mstate_funcall (branch parse-tree) state gotos) gotos))
     (else (error 'interpret-parse-tree (branch parse-tree) "unrecognized branch in parse tree")))))

;; Add closure for the function definition
(define Mstate_func-def
  (lambda (func-name formal-params body state gotos)
    (Mstate_update-var func-name (create_closure func-name formal-params body) (Mstate_insert-var func-name state))))

;; Evaluates state after function call statement
(define Mstate_funcall
  (lambda (func-call state gotos)
    (begin (Mvalue_expression func-call state (gotos/new-throw (lambda (e func-env) ((throw-goto gotos) e state)) gotos)) state)))

;; Creates function closure to bind to variable name
(define create_closure
  (lambda (func-name formal-params body)
    (lambda (actual-params funcall-env gotos)
      (Mvalue_return (Mstate_replace-bools
                      (call/cc
                       (lambda (return)
                         (begin
			   (Mstate body (Mstate_create-env func-name formal-params actual-params funcall-env) (gotos/new-return return gotos))))))))))

;; Creates environment in which to evaluate function 
(define Mstate_create-env
  (lambda (func-name formal-params actual-params state)
    (foldr (lambda (l state) (Mstate_update-var (car l) (cdr l) (Mstate_insert-var (car l) state)))
	   (Mstate_add-layer init-layer (Mstate_funcall-env func-name state))
	   (map cons formal-params actual-params))))

;; Find environment in which function definition resides
(define Mstate_funcall-env
  (lambda (func-name state)
    (cond
     ((null? state) (error 'Mstate_funcall-env "Function not found in state"))
     ((layer_contains-var? func-name (current-layer state)) state)
     (else (Mstate_funcall-env func-name (Mstate_shed-layer state))))))

;; insert the 'return var into the state
(define Mstate_return-stmt
  (lambda (return-stmt state gotos)
    ((return-gotos) (Mstate_var-declaration-stmt 'return return-stmt state gotos))))


;; declaration
(define Mstate_var-declaration-stmt
  (lambda (variable expression state gotos)
    (Mstate_update-var variable (if (null? expression)
				    expression
				    (Mvalue_expression expression state gotos))
		       (Mstate_insert-var variable state))))

;; assigment
(define Mstate_assignment-stmt
  (lambda (variable value state gotos)
    (Mstate_update-var variable value state)))

;; if else
(define Mstate_if-else-stmt
  (lambda (condition then-stmt else-stmt state gotos)
    (if (Mvalue_expression condition state gotos)
	(Mstate (list then-stmt) state gotos)
	(if (null? else-stmt)
	    state
	    (Mstate (list else-stmt) state gotos)))))

;; while
(define Mstate_while-stmt
  (lambda (condition do-stmt state gotos)
    (call/cc
     (lambda (break)
       (letrec ((Mstate_while-loop
		 (lambda (condition do-stmt state gotos)
		   (if (Mvalue_expression condition state gotos)
		       (Mstate_while-loop condition do-stmt
					  (call/cc
					   (lambda (continue)
					     (Mstate (list do-stmt) state (gotos/new-continue continue gotos)))) gotos)
		       state))))
	 (Mstate_while-loop condition do-stmt state (gotos/new-break break gotos)))))))

;; Call with (Mstate_stmt-block stmt-block (Mstate_push-layer init-layer state)))
(define Mstate_stmt-block
  (lambda (stmt-block state gotos)
    (cond
     ((null? stmt-block) (Mstate_pop-layer state))
     (else (Mstate_stmt-block (cdr stmt-block) (Mstate (list (car stmt-block)) state gotos) gotos)))))

;; try block
(define Mstate_try-stmt
  (lambda (body catch finally state gotos)
    (if (null? finally)
	(Mstate_try-catch body catch state gotos)
        (Mstate_finally (cadr finally) (Mstate_try-catch body catch state gotos) gotos))))

(define catch-var (lambda (catch-stmt) (car (first-param catch-stmt))))
(define catch-body caddr)

(define Mstate_try-catch
  (lambda (body catch state gotos)
    (Mstate_pop-layer
     (call/cc
      (lambda (catch-cc)
	(Mstate body state (if (null? catch)
			       gotos
			       (gotos/new-throw (lambda (e-value state) (catch-cc (Mstate_catch e-value (catch-var catch) (catch-body catch) state gotos)))
						gotos))))))))

(define Mstate_finally
  (lambda (body state gotos)
    (Mstate_pop-layer (Mstate body (Mstate_push-layer init-layer state) gotos))))

(define Mstate_catch
  (lambda (e-value e-param body state gotos)
    (Mstate body (Mstate_var-declaration-stmt e-param e-value (Mstate_push-layer init-layer state) gotos) gotos)))

(define Mstate_throw
  (lambda (e state gotos)
    ((throw-goto gotos) (Mvalue_expression e state gotos) (Mstate_pop-layer state))))

;; Calls break continuation on state
(define Mstate_break
  (lambda (state gotos)
    ((break-goto gotos) (Mstate_pop-layer state))))

;; Calls continue continuation on state
(define Mstate_continue
  (lambda (state gotos)
    ((continue-goto gotos) state)))

;; insert the 'return var into the state
(define Mstate_return-stmt
  (lambda (return-stmt state gotos)
    ((return-goto gotos) (Mstate_var-declaration-stmt 'return return-stmt state gotos))))

;; =============================================================================
;;  "gotos" abstractions
;; =============================================================================

(define return-goto car)
(define break-goto cadr)
(define continue-goto caddr)
(define throw-goto cadddr)

(define init-gotos
  (list
   (lambda (v state) (error 'goto-error "return goto has not been set"))
   (lambda (state) (error 'goto-error "break goto has not been set"))
   (lambda (state) (error 'goto-error "continue goto has not been set"))
   (lambda (v state) (error 'goto-error "throw goto has not been set"))))

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
        (cons (car list) (update-at-index (- index 1) new-element (cdr list))))))

;; =============================================================================
;;                                 Helpers
;; =============================================================================

(define vars car)
(define vals cadr)
(define init_var_state list)
(define init-layer (list (list) (list)))
(define current-layer car)
(define Mstate_pop-layer cdr)
(define Mstate_shed-layer cdr)

;; Appends a new working layer to the front of the state
(define Mstate_add-layer (lambda (layer state) (cons layer state)))

;; Appends a new working layer to the front of the state
(define Mstate_push-layer (lambda (layer state) (cons layer state)))

;; replaces occurences of #t with 'true and #f with 'false
(define Mstate_replace-bools
  (lambda (state)
    (replaceall*-cps #t 'true state (lambda (v) (replaceall*-cps #f 'false v (lambda (w) w))))))

(define replaceall*-cps
  (lambda (a b l return)
    (cond
     ((null? l) (return l))
     ((list? (car l)) (replaceall*-cps a b (car l) (lambda (v) (replaceall*-cps a b (cdr l) (lambda (w) (return (cons v w)))))))
     ((eq? a (car l)) (replaceall*-cps a b (cdr l) (lambda (v) (return (cons b v)))))
     (else (replaceall*-cps a b (cdr l) (lambda (v) (return (cons (car l) v))))))))

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

;; takes a variable, a value, and a state and updates the value of the
;; variable, returning the state; produces an error if variable not declared
(define Mstate_update-var
  (lambda (variable value state)
    (if (eq? value 'void)
	(error 'Mstate_update-var "Attempting to assign to void")
	(begin (set-box! (lookup-box variable state) value) state))))

;; takes a variable and a state and returns the value of that variable
;; if it exists and is not null, otherwise produces an error
(define lookup-var
  (lambda (variable state)
    (unbox (lookup-box variable state))))

(define lookup-box
  (lambda (variable state)
    (cond
     ((null? state) (error 'lookup-box "variable name not found"))
     ((layer_contains-var? variable (current-layer state)) (layer_lookup-box variable (current-layer state)))
     (else (lookup-box variable (Mstate_pop-layer state))))))

;; takes a variable and state and returns the state with the vairable
;; initialized to the empty list
(define Mstate_insert-var
  (lambda (variable state)
    (Mstate_add-layer (layer_add-binding variable (box init_var_state) (current-layer state)) (Mstate_shed-layer state))))

;; takes a variable and state and returns true if variable is a member
;; of the state, otherwise returns false
(define contains-var?
  (lambda (variable state)
    (if (contains-var? variable state)
	(error 'Mstate_insert-var "Attempt to insert a var that already exits")
	(Mstate_push-layer (layer_add-binding variable init_var_state (current-layer state)) (Mstate_pop-layer state)))))

;; takes a variable and state and returns true if variable is a member
;; of the state, otherwise returns false
(define contains-var?
  (lambda (variable state)
    (cond
     ((null? state) #f)
     ((layer_contains-var? variable (current-layer state)) #t)
     (else (contains-var? variable (Mstate_pop-layer state))))))

;; =============================================================================
;;  layer functions -- PRIVATE
;; =============================================================================

;; wraps a list of variables and list of values into a new "layer"
(define layer_construct
  (lambda (variables values)
    (list variables values)))

;; takes a variable and a state layer and returns the value of the variable
;; unless it is null.
(define layer_lookup-box
  (lambda (variable layer)
    (cond
     ((null? layer) (error 'layer_lookup-box "variable name not found in layer")) ; Shouldn't get called either
     ((eq? variable (car (vars layer))) (car (vals layer)))
     (else (layer_lookup-box variable (layer_construct (cdr (vars layer)) (cdr (vals layer))))))))

;; takes a variable and layer and returns true if variable is a member
;; of the layer, otherwise returns false
(define layer_contains-var?
  (lambda (variable layer)
    (member variable (vars layer))))

;; adds a new binding onto the current layer, UNSAFE
(define layer_add-binding
  (lambda (variable value layer)
    (if (layer_contains-var? variable layer)
	(error 'layer_add-binding "Variable exists in current environment")
	(layer_construct (cons variable (vars layer)) (cons value (vals layer))))))
