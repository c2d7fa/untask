#lang racket

(require rackunit rackunit/gui
         untask/test/test-integration
         untask/test/test-serialization
         untask/test/test-dependencies
         untask/test/test-links)

(test/gui #:wait? #t
 (test-suite
  "Tests"
  integration-tests
  serialization-tests
  (test-suite
   "Properties"
   dependencies-tests
   links-tests)))
