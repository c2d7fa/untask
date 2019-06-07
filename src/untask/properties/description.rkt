#lang racket

(provide (all-defined-out))

(require
 (prefix-in prop: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt"))

(define description-property-type
  (prop:make-property-type #:key 'description
                           #:default (val:make-string "")))
