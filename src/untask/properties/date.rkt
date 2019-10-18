#lang racket

(provide wait-property
         date-property
         wait-active?)

(require
 (prefix-in p: "../core/property.rkt")
 (prefix-in val:  "../core/value.rkt")
 (prefix-in dt: "../../datetime.rkt"))

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
