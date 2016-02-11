; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(load "jupiter")

(define test
    (lambda ()
        (display
            (string-append
                (assert-equal "tests/1/01" 150)
                (assert-equal "tests/1/02" (- 4))
                (assert-equal "tests/1/03" 10)
                (assert-equal "tests/1/04" 16)
                (assert-equal "tests/1/05" 220)
                (assert-equal "tests/1/06" 5)
                (assert-equal "tests/1/07" 6)
                (assert-equal "tests/1/08" 10)
                (assert-equal "tests/1/09" 5)
                (assert-equal "tests/1/10" (- 39))
                ; tests/1/11 should produce error
                ; tests/1/12 should produce error
                ; tests/1/13 should produce error
                ; tests/1/14 should produce error
                (assert-equal "tests/1/15" 'true)
                (assert-equal "tests/1/16" 100)
                (assert-equal "tests/1/17" 'false)
                (assert-equal "tests/1/18" 'true)
                ))))

(define assert-equal
    (lambda (file expected-value)
        (if (eq? (interpret file) expected-value)
            (string-append file " passed\n")
            (string-append file " failed\n"))))
