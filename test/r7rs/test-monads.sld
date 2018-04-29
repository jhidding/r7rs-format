(import (scheme base)
        (scheme write)
        (assertions)
        (tests)
        (monads))

(define (div x y)
  (if (= 0 y)
      *nothing*
      (/ x y)))
(define (test-seq-maybe)
  (define (f x)
    (seq <maybe>
         (a <- (div 1 x))
         (b <- (+ a 1))
         b))

  (assert (nothing? (f 0)))
  (assert= (f 3) 4/3))

(display (run-test test-seq-maybe))
(newline)
