; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

(load "jupiter")

(define test
    (lambda ()
        (display
            (if (string=? (string-append (test-1)) "")
                "\nAll tests passed!\n\n"
                (string-append (test-1))))))

(define test-1
    (lambda ()
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
            (assert-equal "tests/1/19" 128)
            (assert-equal "tests/1/20" 12))))

(define test-2
    (lambda ()
        (string-append
            (assert-equal "tests/2/01" 20)
            (assert-equal "tests/2/02" 164)
            (assert-equal "tests/2/03" 32)
            (assert-equal "tests/2/04" 2)
            ; tests/2/05 should produce error
            (assert-equal "tests/2/06" 25)
            (assert-equal "tests/2/07" 21)
            (assert-equal "tests/2/08" 6)
            (assert-equal "tests/2/09" (- 1))
            (assert-equal "tests/2/10" 789)
            ; tests/2/11 should produce error
            ; tests/2/12 should produce error
            ; tests/2/13 should produce error
            (assert-equal "tests/2/14" 12)
            (assert-equal "tests/2/15" 125)
            (assert-equal "tests/2/16" 110)
            (assert-equal "tests/2/17" 2000400)
            (assert-equal "tests/2/18" 101)
            ; tests/2/19 should produce error
            )))

(define assert-equal
    (lambda (file expected-value)
        (if (eq? (interpret file) expected-value)
            ""
            (string-append file " FAILED\n"))))
