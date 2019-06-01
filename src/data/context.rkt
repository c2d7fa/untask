#lang racket

(provide (all-defined-out))

(define context-definitions-empty
  (hash))

(define (context-definitions-available definitions)
  (hash-keys definitions))

(define (context-definitions-define definitions name filter-expression modify-expression)
  (hash-set definitions name (list filter-expression modify-expression)))

(define (context-definitions-remove definitions name)
  (hash-remove definitions name))

(define (context-definitions-get definitions name)
  (hash-ref definitions
            name
            '((and) (and))))

(define (apply-context-to-filter-expression context filter-expression)
  `(and ,(car context) ,filter-expression))

(define (apply-context-to-modify-expression context modify-expression)
  `(and ,(cadr context) ,modify-expression))
