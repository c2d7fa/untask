#lang racket

(require rackunit
         "util.rkt"
         "../src/untask/core/serialize.rkt"

         "../src/untask/core/state.rkt"
         (prefix-in item: "../src/untask/core/item.rkt")
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
     (define item-data (a:get (st state.item-data)))
     (check-equal? (item:get-property item-data 1 description-property-type) (val:make-string "Task 1"))
     (check-equal? (item:get-property item-data 2 description-property-type) (val:make-string "Task 2"))
     (check-equal? (item:get-property item-data 3 blocks-property-type) (val:make-set (set (val:make-item 2)))))
   (test-case "Loading saved data gives original result"
     (define st (load-state "./example.t"))
     (check-equal? st (load-state-from-string #:open-file "./example.t" (save-state-to-string st))))
   ))
