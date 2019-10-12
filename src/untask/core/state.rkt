#lang racket

(provide (all-defined-out))

(require
 (prefix-in item: "./item.rkt")
 (prefix-in context: "./context.rkt")
 (prefix-in a: "../../attribute.rkt"))

(a:define-species state (context-state open-file item-data))

(define state-empty
  (state #:context-state context:empty-state
         #:open-file #f
         #:item-data item:item-data-empty))
