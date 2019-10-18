#lang racket

(require rackunit rackunit/gui
         "test-integration.rkt"
         "test-serialization.rkt"
         "test-dependencies.rkt"
         "test-links.rkt")

(test/gui #:wait? #t
 (test-suite
  "Tests"
  integration-tests
  serialization-tests
  (test-suite
   "Properties"
   dependencies-tests
   links-tests)))
