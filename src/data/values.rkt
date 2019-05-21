#lang racket

(provide (all-defined-out))

(require
 (prefix-in expr: "expressions.rkt"))

(define (make-string string-content)
  `(string . ,string-content))
(define (make-set (set-value (set)))
  `(set . ,set-value))
(define (make-number x)
  `(number . ,x))
(define (make-item id)
  `(item . ,id))
(define (make-boolean v)
  `(boolean . ,v))

(define unwrap-string cdr)
(define unwrap-set cdr)
(define unwrap-number cdr)
(define unwrap-item cdr)
(define unwrap-boolean cdr)

(define (evaluate-literal literal-expression)
  (match literal-expression
    (`(string . ,string-content) `(string . ,string-content))
    (`(number . ,number-value) `(number . ,number-value))
    (`(item . ,item-id) `(item . ,item-id))
    (`(set . ,set-expressions) `(set . ,(list->set (map evaluate-literal set-expressions))))
    (`(boolean . ,boolean-value) (make-boolean boolean-value))
    ))