#lang racket

(require
 rackunit/text-ui

 "./test/unit/user/parser.rkt"
 "./test/integration.rkt"
 )

(run-tests parser-tests
           integration-tests)
