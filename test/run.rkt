#lang racket

(require rackunit rackunit/gui
         "test-integration.rkt"
         "test-serialization.rkt")

(test/gui #:wait? #t
          (test-suite "Tests" integration-tests serialization-tests))
