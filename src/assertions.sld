(define-library (assertions)
  (export assert assertion-error? assertion-error-msg assertion-error-irritants
	  assert-comp assert-eq assert-equal assert=)
  (import (scheme base))

  (begin
    (define-record-type <assertion-error>
      (assertion-error msg irritants)
      assertion-error?
      (msg assertion-error-msg)
      (irritants assertion-error-irritants))

    (define-syntax assert
      (syntax-rules ()
	((_ <x>)
	 (if <x>
	     #t
	     (raise (assertion-error "Assertion failed: {:s}\n" '(<x>)))))))

    (define-syntax assert-comp
      (syntax-rules ()
	((_ <comp> <a> <b>)
	 (let ((a <a>)
	       (b <b>))
	   (if (<comp> a b)
	       #t
	       (raise (assertion-error
		       "Not {:s}:\n* {:s}\nevaluated to\n* {:s}\nexpected\n* {:s}\n"
		       (list '<comp> '<a> a b))))))))

    (define-syntax assert-eq
      (syntax-rules ()
	((_ <a> <b>)
	 (assert-comp eq? <a> <b>))))

    (define-syntax assert-equal
      (syntax-rules ()
	((_ <a> <b>)
	 (assert-comp equal? <a> <b>))))

    (define-syntax assert=
      (syntax-rules ()
	((_ <a> <b>)
	 (assert-comp = <a> <b>))))
))
