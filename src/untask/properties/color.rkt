#lang racket

(provide color-property)

(require (prefix-in p: untask/src/untask/core/property))

(define color-property
  (p:property #:name 'color
              #:type '(enum red green yellow blue magenta cyan)))
