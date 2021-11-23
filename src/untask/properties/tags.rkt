#lang racket

(provide (all-defined-out))

(require
 (prefix-in p: "../../untask/core/property.rkt")
 (prefix-in val: "../../untask/core/value.rkt")
 "../../squiggle.rkt")

(define tags-property
  (~> (p:property #:name 'tags
                  #:type '(set string))
      (p:default (val:make-set))))
