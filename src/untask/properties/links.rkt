#lang racket

(provide children-property parents-property)

(require
 untask/src/untask/properties/backlinks
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in val: untask/src/untask/core/value)
 untask/src/squiggle)

(define children-property
  (~> (p:property #:name 'children
                  #:type '(set item))
      (p:default (val:make-set))))

(define parents-property
  (p:property #:name 'parents
              #:type '(set any)
              #:calculate (calculate-backlinks children-property)
              #:translate (translate-backlinks children-property)))
