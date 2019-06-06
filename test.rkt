#lang racket

(require
 rackunit/text-ui

 "./test/user/parser.rkt"
 "./test/integration.rkt"
 )

(run-tests parser-tests
           integration-tests)
