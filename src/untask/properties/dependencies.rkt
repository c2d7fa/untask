#lang racket

(provide depends-property
         blocks-property)

(require
 "backlinks.rkt"
 (prefix-in p: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt")
 "../../squiggle.rkt")

(define depends-property
  (~> (p:property #:name 'depends
                  #:type '(set item))
      (p:default (val:make-set))))

(define blocks-property
  (p:property #:name 'blocks
              #:type '(set any)
              #:calculate (calculate-backlinks depends-property)
              #:translate (translate-backlinks depends-property)))
