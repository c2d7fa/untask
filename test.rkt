#lang racket

(require
 rackunit
 rackunit/text-ui

 "./test/unit/user/parser.rkt"
 "./test/integration.rkt"
 )

(run-tests
 (test-suite "untask"
             parser-tests
             integration-tests))
