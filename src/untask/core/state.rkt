#lang racket

(provide (all-defined-out))

(require
 (prefix-in i: "./item.rkt")
 (prefix-in c: "./context.rkt")
 (prefix-in a: "../../attribute.rkt"))

(a:define-species state (context-state item-state open-file))

(define state-empty
  (state #:context-state c:empty-state
         #:item-state i:empty-state
         #:open-file #f))
