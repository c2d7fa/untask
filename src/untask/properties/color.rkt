#lang racket

(provide color-property)

(require (prefix-in p: "../../untask/core/property.rkt"))

(define color-property
  (p:property #:name 'color
              #:type '(opt (enum red green yellow blue magenta cyan))))
