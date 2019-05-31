#lang racket

(provide (all-defined-out))

(require
 (prefix-in item: "item-data.rkt")
 (prefix-in context: "context.rkt"))

(define state-empty
  (list*
   context:context-definitions-empty ; Defined contexts
   #f                                ; Current context
   item:item-data-empty))            ; Item data

(define (state-defined-contexts st) (car st))
(define (state-current-context st) (cadr st))
(define (state-item-data st) (cddr st))

(define (state-set-defined-contexts st x)
  (list* x (state-current-context st) (state-item-data st)))
(define (state-set-current-context st x)
  (list* (state-defined-contexts st) x (state-item-data st)))
(define (state-set-item-data st x)
  (list* (state-defined-contexts st) (state-current-context st) x))
