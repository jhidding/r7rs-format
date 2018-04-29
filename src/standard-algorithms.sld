(define-library (standard-algorithms)
  (export append-reverse fold-left fold-right string-join)
  (import (scheme base))

  (begin
    (define (append-reverse rev-head tail)
      (if (null? rev-head)
	  tail
	  (append-reverse
	   (cdr rev-head)
	   (cons (car rev-head) tail))))

    (define (fold-left f acc . lsts)
      (if (null? (car lsts))
	  acc
	  (apply
	   fold-left
	   f
	   (apply f acc (map car lsts))
	   (map cdr lsts))))

    (define (fold-right f acc . lsts)
      (reverse
       (fold-left
	(lambda (acc . args)
	  (apply f (append-reverse args (list acc))))
	acc
	(map reverse lsts))))

    (define (string-join lst sep)
      (do ((result ""   (string-append result sep (car lst)))
	   (lst    lst  (cdr lst)))
	  ((null? lst) result)))

    ))
