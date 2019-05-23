#lang racket

(provide (all-defined-out))

(require
  (prefix-in prop: "../data/property-type.rkt")
  (prefix-in val: "../data/values.rkt"))

(define depends-property-type
  (prop:make-property-type #:key 'depends
                           #:default (val:make-set)))
