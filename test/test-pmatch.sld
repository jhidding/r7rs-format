(import (scheme base)
        (scheme write)

        (assertions)
        (tests)

        (pmatch))

(define (test-pmatch)
  (assert-equal
   (pmatch '(1 2 3)
     ((,a . ,b) b))
   '(2 3)))

(display (run-test test-pmatch)) (newline)
