#lang racket

(provide (all-defined-out))

(require
 (prefix-in item: "./item.rkt")
 (prefix-in context: "./context.rkt")
 (prefix-in a: "../../attribute.rkt"))

(a:define-record state (defined-contexts active-contexts item-data))

(define state-empty
  (state #:defined-contexts context:empty-contexts
         #:active-contexts (set)
         #:item-data item:item-data-empty))
