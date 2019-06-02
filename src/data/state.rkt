#lang racket

(provide (all-defined-out))

(require
 (prefix-in item: "item-data.rkt")
 (prefix-in context: "context.rkt")
 (prefix-in a: "../util/attributes.rkt"))

(define state-empty
  (list*
   context:empty-contexts ; Defined contexts
   (set)                  ; Current contexts
   item:item-data-empty)) ; Item data

(define (state-defined-contexts st) (car st))
(define (state-current-contexts st) (cadr st))
(define (state-item-data st) (cddr st))

(define (state-set-defined-contexts st x)
  (list* x (state-current-contexts st) (state-item-data st)))
(define (state-set-current-contexts st x)
  (list* (state-defined-contexts st) x (state-item-data st)))
(define (state-set-item-data st x)
  (list* (state-defined-contexts st) (state-current-contexts st) x))
