#lang racket

(require rackunit rackunit/gui
         "test-integration.rkt"
         "test-serialization.rkt"
         "test-dependencies.rkt"
         "test-links.rkt"
         "test-item.rkt")

(test/gui
 (test-suite
  "Tests"
  integration-tests
  (test-suite "Core modules" item-tests serialization-tests)
  (test-suite "Properties" dependencies-tests links-tests))
 #:wait? #t)
