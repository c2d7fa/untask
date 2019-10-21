#lang racket

(provide (all-defined-out))

(require
 (prefix-in i: untask/src/untask/core/item)
 (prefix-in c: untask/src/untask/core/context)
 (prefix-in a: untask/src/attribute))

(a:define-species state (context-state item-state open-file))

(define state-empty
  (state #:context-state c:empty-state
         #:item-state i:empty-state
         #:open-file #f))
