#lang racket

(provide effort-property)

(require untask/src/squiggle
         (prefix-in p: untask/src/untask/core/property)
         (prefix-in v: untask/src/untask/core/value))

(define effort-property
  (~> (p:property #:name 'effort
                  #:type 'number)
      (p:default (v:make-number 0))))
