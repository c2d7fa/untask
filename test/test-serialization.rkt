#lang racket

(require rackunit
         "util.rkt"
         "../src/untask/core/serialize.rkt"

         "../src/untask/core/state.rkt"
         (prefix-in p: "../src/untask/core/property.rkt")
         "../src/untask/properties/description.rkt"
         "../src/untask/properties/dependencies.rkt"
         (prefix-in a: "../src/attribute.rkt")
         (prefix-in val: "../src/untask/core/value.rkt"))

(provide serialization-tests)

(define serialization-tests
  (test-suite
   "Serialization"
   (test-case "Loading file produces some correct item data"
     (define st (load-state "./example.t"))
     (define item-state (a:get-path (st state.item-state)))
     (check-equal? (p:get item-state 1 description-property) (val:make-string "Task 1"))
     (check-equal? (p:get item-state 2 description-property) (val:make-string "Task 2"))
     (check-equal? (p:get item-state 3 blocks-property) (val:make-set (set (val:make-item 2)))))
   (test-case "Loading saved data gives original result"
     (define st (load-state "./example.t"))
     (check-equal? st (load-state-from-string #:open-file "./example.t" (save-state-to-string st))))
   ))
