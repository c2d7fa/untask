#lang racket

(provide description-property
         notes-property)

(require
 (prefix-in p: "../../untask/core/property.rkt")
 (prefix-in val: "../../untask/core/value.rkt")
 "../../squiggle.rkt")

(define description-property
  (~> (p:property #:name 'description
                  #:type 'string)
      (p:default (val:make-string ""))))

(define notes-property
  (~> (p:property #:name 'notes
                  #:type 'string)
      (p:default (val:make-string ""))))
