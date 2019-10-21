#lang racket

(provide (all-defined-out))

(require
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in val: untask/src/untask/core/value)
 untask/src/squiggle)

(define tags-property
  (~> (p:property #:name 'tags
                  #:type '(set string))
      (p:default (val:make-set))))
