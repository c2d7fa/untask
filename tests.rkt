#lang racket

(require rackunit
         racket/enter
         rackunit-fancy-runner

         untask/test/test-check-expression
         untask/test/test-command
         untask/test/test-context
         untask/test/test-date
         untask/test/test-dependencies
         untask/test/test-integration
         untask/test/test-item
         untask/test/test-links
         untask/test/test-order
         untask/test/test-parser
         untask/test/test-serialization
         untask/test/test-value)

(provide all-tests test!)

(define all-tests
  (test-suite "Tests"
    integration-tests
    command-tests
    check-expression-tests
    serialization-tests
    value-tests
    item-tests
    context-tests
    (test-suite "Properties"
      dependencies-tests
      links-tests
      date-tests
      order-tests)
    parser-tests))

(define (test!)
  (run-tests/fancy all-tests))

;; For interactive use. To use this, enter this module (with ",enter
;; tests.rkt"), and then run (retest!) after each change. This reduces the
;; impact of Racket's start-up time, so that unit tests are run faster.
(define (retest!)
  (enter! untask/tests)
  (test!))

(module+ main
  (test!))
