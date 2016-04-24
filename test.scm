; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(load "jupiter.scm")

(define test
    (lambda ()
        (display
            (if (string=? (string-append (test-4)) "")
                "\nAll tests passed!\n\n"
                (string-append (test-4))))))

(define test-4
    (lambda ()
        (string-append
            (assert-equal "tests/4/01" "A" 15)
            (assert-equal "tests/4/02" "A" 12)
            (assert-equal "tests/4/03" "A" 125)
            (assert-equal "tests/4/04" "A" 36)
            (assert-equal "tests/4/05" "A" 54)
            (assert-equal "tests/4/06" "A" 110)
	         ;(assert-equal "tests/4/07" "C" 26)
            )))

(define assert-equal
    (lambda (file class-name expected-value)
        (if (eq? (interpret file class-name) expected-value)
            ""
            (string-append file " FAILED\n"))))
