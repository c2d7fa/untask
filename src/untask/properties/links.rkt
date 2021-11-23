#lang racket

(provide children-property parents-property)

(require
 "../../untask/properties/backlinks.rkt"
 (prefix-in p: "../../untask/core/property.rkt")
 (prefix-in val: "../../untask/core/value.rkt")
 "../../squiggle.rkt")

(define children-property
  (~> (p:property #:name 'children
                  #:translate (translate-links 'children 'parents)
                  #:type '(set item))
      (p:default (val:make-set))))

(define parents-property
  (~> (p:property #:name 'parents
                  #:type '(set any)
                  #:translate (translate-backlinks 'children 'parents))
      (p:default (val:make-set))))
