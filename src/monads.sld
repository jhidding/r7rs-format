(define-library (monads)
  (export seq make-monad monad? monad-return monad-bind

          *maybe* *nothing* nothing?)
  (import (scheme base))

  (begin
    (define-record-type <monad>
      (make-monad bind return)
      monad?
      (bind monad-bind)
      (return monad-return))

    (define-syntax seq
      (syntax-rules (<-)
        ;; the last expression in a sequence remains as is.
        ((_ <M> <f>)
         <f>)

        ;; (seq M (a <- expression) ...) expands to a nested
        ;; binding to a function that contains the rest of the
        ;; sequence
        ((_ <M>
            (<formals> ... <- <f>)
            <rest> ...)

         ((monad-bind <M>)
          <f>
          (lambda (<formals> ...)
            (seq <M> <rest> ...))))

        ;; If the pattern doesn't match the (a <- expr) pattern,
        ;; the outcome of <f> is thrown away, but we still need
        ;; a lambda for bind to work on.
        ((_ <M>
            <f>
            <rest> ...)

         ((monad-bind <M>)
          <f>
          (lambda _
            (seq <M> <rest> ...))))))

    (define-record-type <nothing>
      (make-nothing)
      nothing?)

    (define *nothing* (make-nothing))

    (define (maybe-bind value f)
      (if (nothing? value)
          value
          (f value)))

    (define maybe-return values)

    (define *maybe* (make-monad maybe-bind maybe-return))
 ))
