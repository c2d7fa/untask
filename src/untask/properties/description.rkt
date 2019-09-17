#lang racket

(provide (all-defined-out))

(require
 (prefix-in prop: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt"))

(define description-property-type
  (prop:property-type #:key 'description
                      #:type 'string
                      #:default (val:make-string "")))

(define notes-property-type
  (prop:property-type #:key 'notes
                      #:type 'string
                      #:default (val:make-string "")))
