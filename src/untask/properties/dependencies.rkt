#lang racket

(provide depends-property
         blocks-property)

(require
 untask/src/untask/properties/backlinks
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in val: untask/src/untask/core/value)
 untask/src/squiggle)

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
