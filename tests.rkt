#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         racket/enter

         untask/test/test-item
         untask/test/test-context
         untask/test/test-dependencies
         untask/test/test-integration
         untask/test/test-links
         untask/test/test-serialization
         untask/test/test-parser)

(provide all-tests test!)

(define all-tests
  (test-suite "Tests"
    integration-tests
    serialization-tests
    item-tests
    context-tests
    (test-suite "Properties"
      dependencies-tests
      links-tests)
    parser-tests))

(define (test!)
  (run-tests all-tests 'verbose))

;; For interactive use. To use this, enter this module (with ",enter
;; tests.rkt"), and then run (retest!) after each change. This reduces the
;; impact of Racket's start-up time, so that unit tests are run faster.
(define (retest!)
  (enter! untask/tests)
  (test!))

(define (retest!/gui)
  (enter! untask/tests)
  (test/gui #:wait? #t all-tests))
