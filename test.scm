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
            (assert-equal "tests/3/01" 10)
            (assert-equal "tests/3/02" 14)
            (assert-equal "tests/3/03" 45)
            (assert-equal "tests/3/04" 55)
            (assert-equal "tests/3/05" 1)
            (assert-equal "tests/3/06" 115)
	    (assert-equal "tests/3/07" 'true)
            (assert-equal "tests/3/08" 20)
            (assert-equal "tests/3/09" 24)
            (assert-equal "tests/3/10" 2)
            (assert-equal "tests/3/11" 35)
            ;; (assert-equal "tests/3/12" 'error)
            (assert-equal "tests/3/13" 90)
            (assert-equal "tests/3/14" 69)
            (assert-equal "tests/3/15" 87)
            (assert-equal "tests/3/16" 64)
            ;; (assert-equal parser "tests/3/17" 20)
            (assert-equal "tests/3/18" 125)
            (assert-equal "tests/3/19" 100)
            (assert-equal "tests/3/20" 2000400)
	    (assert-equal "tests/3/21" 3421)
            (assert-equal "tests/3/22" 20332)
	    (assert-equal "tests/3/23" 21)
            )))

(define assert-equal
    (lambda (file expected-value)
        (if (eq? (interpret file) expected-value)
            ""
            (string-append file " FAILED\n"))))
