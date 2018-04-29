(library (tests)
  (export run-test)
  (import (except (rnrs (6)) assert)
	  (assertions))

  (define (run-test t)
    (call-with-current-continuation
     (lambda (return)
       (with-exception-handler
  	 (lambda (exc)
  	   (if (assertion-violation? exc)
  	       (begin
  		 (display (condition-message exc))
  		 (newline)
  		 (display (condition-irritants exc))
  		 (newline))
  	       (begin
  		 (display "Unknown exception: ")
  		 (display exc)
  		 (newline)))
  	   (return 'fail))
         t)
       'success)))
)
