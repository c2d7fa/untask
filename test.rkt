#lang racket

(require
 rackunit/gui
 "./test/user/parser.rkt")

(test/gui parser-tests #:wait? #t)
