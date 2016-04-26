;;Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;;Case Western Reserve PLC Spring 2016

(load "jupiter.scm")

(define test
  (lambda ()
    (display
     (if (string=? (string-append (test-3)) "")
	 "\nAll tests passed!\n\n"
	 (string-append (test-3))))))

(define test-4
    (lambda ()
        (string-append
            (assert-equal "tests/4/01" "A" 15)
            (assert-equal "tests/4/02" "A" 12)
            (assert-equal "tests/4/03" "A" 125)
            (assert-equal "tests/4/04" "A" 36)
            (assert-equal "tests/4/05" "A" 54)
            (assert-equal "tests/4/06" "A" 110)
	         (assert-equal "tests/4/07" "C" 26)
            (assert-equal "tests/4/08" "Square" 117)
            (assert-equal "tests/4/09" "Square" 32)
            (assert-equal "tests/4/10" "List" 15)
            
            )))

(define assert-equal
  (lambda (file expected-value)
    (if (eq? (interpret file) expected-value)
	""
	(string-append file " FAILED\n"))))
