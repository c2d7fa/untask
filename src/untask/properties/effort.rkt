#lang racket

(provide effort-property)

(require "../../squiggle.rkt"
         (prefix-in p: "../../untask/core/property.rkt")
         (prefix-in v: "../../untask/core/value.rkt"))

(define effort-property
  (~> (p:property #:name 'effort
                  #:type 'number)
      (p:default (v:make-number 0))))
