#lang racket

(require rackunit
         racket/exn

         ;; We need this in order to be able to inspect the check-info results.
         (prefix-in priv: rackunit/private/check-info))

(provide run-tests/fancy)

(define (indent text #:spaces (spaces 4))
  (define (relines lines) (string-join lines "\n"))
  (define (unlines text) (string-split text "\n"))
  (define (indent-line line) (format "~A~A" (make-string spaces #\space) line))
  (relines (map indent-line (unlines text))))

(define (run-tests/fancy tests)
  (define (format-srclocs result-exn)
    (if (exn:srclocs? result-exn)
      (let ((srclocs ((exn:srclocs-accessor result-exn) result-exn)))
        (string-join (map srcloc->string srclocs) "; "))
      ""))
  (define (format-test-result result)
    (define test-case-name (test-result-test-case-name result))
    (cond
      ((test-failure? result)
       (define result-exn (test-failure-result result))
       (define description
         (format "The test failed.~%~A"
                 (string-join
                  (map (λ (ci)
                         (define (pad s)
                           (format "~A~A" s (make-string (- 12 (string-length s)) #\space)))
                         (format "\e[30m~A\e[0;1m~A\e[0m"
                                 (pad (format "~A: " (string-titlecase (symbol->string (check-info-name ci)))))
                                 (priv:info-value->string (check-info-value ci))))
                       (exn:test:check-stack result-exn))
                  "\n")))
       (format "\e[31;1m~A\e[0;30m~%~A\e[0m" test-case-name (indent description #:spaces 2)))
      ((test-error? result)
       (define result-exn (test-error-result result))
       (define description
         (format "An error occured:~%\e[35m~A\e[30m"
                 (exn->string result-exn)))
       (format "\e[35;1m~A\e[0;30m~%~A\e[0m" test-case-name (indent description #:spaces 2)))
      ((test-success? result) (format "\e[32;1m~A\e[0m" test-case-name))))
  (define var-bad 0)
  (define var-good 0)
  (void
    (fold-test-results (λ (result state)
                         (if (or (test-failure? result) (test-error? result))
                           (set! var-bad (add1 var-bad))
                           (set! var-good (add1 var-good)))
                         (displayln (indent (format-test-result result) #:spaces (* 2 state)))
                         state)
                       0
                       tests
                       #:fdown (λ (suite-name state)
                                 (displayln (indent (format "\e[1m~A\e[0m"  suite-name) #:spaces (* 2 state)))
                                 (add1 state))
                       #:fup (λ (suite-name state)
                               (sub1 state)))
    (printf "~%Ran \e[1m~A\e[0m tests: \e[34;1m~A\e[0m succeeded, \e[31;1m~A\e[0m failed.~%"
            (+ var-good var-bad)
            var-good
            var-bad)))
