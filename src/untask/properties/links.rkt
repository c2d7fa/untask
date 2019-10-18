#lang racket

(provide children-property parents-property)

(require
 "backlinks.rkt"
 (prefix-in p: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt")
 "../../squiggle.rkt")

(define children-property
  (~> (p:property #:name 'children
                  #:type '(set item))
      (p:default (val:make-set))))

(define parents-property
  (p:property #:name 'parents
              #:type '(set any)
              #:calculate (calculate-backlinks children-property)
              #:translate (translate-backlinks children-property)))
