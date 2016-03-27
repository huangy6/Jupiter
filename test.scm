; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(load "jupiter")

(define test
    (lambda ()
        (display
            (if (string=? (string-append (test-3)) "")
                "\nAll tests passed!\n\n"
                (string-append (test-3))))))

(define test-3
    (lambda ()
        (string-append
            (assert-equal "tests/3/01" 20)
            (assert-equal "tests/3/02" 20)
            (assert-equal "tests/3/03" 20)
            (assert-equal "tests/3/04" 20)
            (assert-equal "tests/3/05" 20)
            (assert-equal "tests/3/06" 20)
            (assert-equal "tests/3/08" 20)
            (assert-equal "tests/3/09" 20)
            (assert-equal "tests/3/10" 20)
            (assert-equal "tests/3/11" 20)
            (assert-equal "tests/3/12" 20)
            (assert-equal "tests/3/13" 20)
            (assert-equal "tests/3/14" 20)
            (assert-equal "tests/3/15" 20)
            (assert-equal "tests/3/16" 20)
            ;(assert-equal parser "tests/3/17" 20)
            (assert-equal "tests/3/18" 20)
            (assert-equal "tests/3/19" 20)
            (assert-equal "tests/3/20" 20)
            )))

(define assert-equal
    (lambda (file expected-value)
        (if (eq? (interpret file) expected-value)
            ""
            (string-append file " FAILED\n"))))
