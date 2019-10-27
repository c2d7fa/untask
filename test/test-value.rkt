#lang racket

(require rackunit
         (prefix-in v: untask/src/untask/core/value)

         (prefix-in dt: untask/src/datetime))

(provide value-tests)

;; Technically these tests will break if the day ends before while the tests are
;; running, but that's probably not an issue...
(define (today+ d)
  (v:make-date (dt:add-days (dt:today) d)))

(define value-tests
  (test-suite "Value"
    (test-case "Evaluating 'Today' literal"
      (check-equal? (v:evaluate-literal '(date today 3)) (today+ 3))
      (check-equal? (v:evaluate-literal '(date today 0)) (today+ 0))
      (check-equal? (v:evaluate-literal '(date today -14)) (today+ -14)))))
