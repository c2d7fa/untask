#lang racket

(provide wait-property
         date-property
         wait-active?)

(require
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in val:  untask/src/untask/core/value)
 (prefix-in dt: untask/src/datetime))

(define wait-property
  (p:property #:name 'wait
              #:type '(opt date)))

(define date-property
  (p:property #:name 'date
              #:type '(opt date)))

(define (wait-active? item-state item)
  (let ((wait (p:get item-state item wait-property)))
    (or (not wait)
        (not (dt:future? (val:unwrap-date wait))))))
