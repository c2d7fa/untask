#lang racket

(provide depends-property
         blocks-property)

(require
 "../../untask/properties/backlinks.rkt"
 (prefix-in p: "../../untask/core/property.rkt")
 (prefix-in val: "../../untask/core/value.rkt")
 "../../squiggle.rkt")

(define depends-property
  (~> (p:property #:name 'depends
                  #:translate (translate-links 'depends 'blocks)
                  #:type '(set item))
      (p:default (val:make-set))))

(define blocks-property
  (~> (p:property #:name 'blocks
                  #:type '(set any)
                  #:translate (translate-backlinks 'depends 'blocks))
      (p:default (val:make-set))))
