#lang racket

(provide (all-defined-out))

(require
 (prefix-in expr: "../command/expression.rkt"))

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

(define (get-type v)
  (let ((type (car v)))
    (if (eq? type 'set)
        (if (set-empty? (cdr v))
            '(set any)
            `(set ,(get-type (car (set->list (cdr v))))))
        type)))
(define (type<=? t1 t2)
  (or (equal? t1 t2)
      (eq? 'any t1)
      (eq? 'any t2)
      (and (list? t1)
           (list? t2)
           (type<=? (cadr t1)
                    (cadr t2))
      )))

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
