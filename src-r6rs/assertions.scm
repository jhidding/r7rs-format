(library (assertions)
  (export assert-comp assert-eq assert-equal assert=)
  (import (rnrs (6)))

  (define-syntax assert-comp
    (syntax-rules ()
      ((_ <comp> <a> <b>)
       (let ((a <a>)
  	   (b <b>))
         (if (<comp> a b)
  	   #t
  	   (raise (make-assertion-violation
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
)
