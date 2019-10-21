#lang racket

(require rackunit
         rackunit/text-ui
         racket/enter

         untask/test/test-dependencies
         untask/test/test-integration
         untask/test/test-links
         untask/test/test-serialization)

(provide all-tests test!)

(define all-tests
  (test-suite "Tests"
    integration-tests
    serialization-tests
    (test-suite "Properties"
      dependencies-tests
      links-tests)))

(define (test!)
  (run-tests all-tests 'verbose))

;; For interactive use. To use this, enter this module (with ",enter
;; tests.rkt"), and then run (retest!) after each change. This reduces the
;; impact of Racket's start-up time, so that unit tests are run faster.
(define (retest!)
  (enter! untask/tests)
  (test!))
