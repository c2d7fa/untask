#lang racket

(provide (all-defined-out))

(require
 (prefix-in expr: "expressions.rkt"))

(define (make-string string-content)
  `(string . ,string-content))
(define (make-set)
  `(set . ,(set)))
(define (make-number x)
  `(number . ,x))

(define (evaluate-literal literal-expression)
  (match literal-expression
    (`(string . ,string-content) `(string . ,string-content))
    (`(number . ,number-value) `(number . ,number-value))
    (`(item . ,item-id) `(item . ,item-id))
    (`(set . ,set-expressions) `(set . ,(list->set (map evaluate-literal set-expressions))))
    ))
