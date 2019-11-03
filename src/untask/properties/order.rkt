#lang racket

(provide order-property)

(require (prefix-in p: untask/src/untask/core/property))

(define order-property
  (p:property #:name 'order
              #:type '(opt number)))
