#lang racket

(provide description-property
         notes-property)

(require
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in val: untask/src/untask/core/value)
 untask/src/squiggle)

(define description-property
  (~> (p:property #:name 'description
                  #:type 'string)
      (p:default (val:make-string ""))))

(define notes-property
  (~> (p:property #:name 'notes
                  #:type 'string)
      (p:default (val:make-string ""))))
