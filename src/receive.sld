(define-library (receive)
  (export receive)
  (import (scheme base))

  ;;; (srfi :8 receive)
  (begin
    (define-syntax receive
      (syntax-rules ()
        ((_ <formals> <expr> <body> ...)
         (call-with-values
             (lambda () <expr>)
           (lambda <formals> <body> ...)))))
))
