M_state
============

## Internal Representation
The state is stored as a list (S1 S2 S3 .. Sn), where each Si has the format ((var1 var2 var3 ...) (val1 val2 val3 ...)). the list of statements in order

## Methods

* `(Mstate stmt-list state)`
    + Evaluates to the state after executing stmt-list in order

* `(Mstate_update-var var new-value state)`
	+ Evaluates to the state with the new value of var at the present scope

* `(lookup-var var state)`
	+ Evaluates to the stored value of var in the present scope 

* `(contains-var? var state)`
	+ Evaluates to #t if var is in scope, or #f if var is out of scope.
