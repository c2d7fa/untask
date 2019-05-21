#lang racket

(provide (all-defined-out))

(define (make-item id)
  `(item . ,id))
(define (make-number x)
  `(number . ,x))
(define (make-string str)
  `(string . ,str))
