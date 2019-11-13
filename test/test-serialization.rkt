#lang racket

(require rackunit untask/test/util
         untask/src/untask/core/serialize

         untask/src/untask/core/state
         (prefix-in i: untask/src/untask/core/item)
         (prefix-in p: untask/src/untask/core/property)
         (prefix-in val: untask/src/untask/core/value)

         untask/src/untask/properties/description
         untask/src/untask/properties/dependencies
         untask/src/untask/properties/order

         (prefix-in a: untask/src/attribute)

         pkg/lib)

(provide serialization-tests)

(define example-path (path->string (build-path (pkg-directory "untask") "test/examples/example-1.t")))

(define serialization-tests
  (test-suite
   "Serialization"

   (test-case "Loading file produces some correct item data"
     (define st (load-state example-path))
     (define item-state (a:get-path (st state.item-state)))
     (check-equal? (p:get item-state 1 description-property) (val:make-string "Task 1"))
     (check-equal? (p:get item-state 2 description-property) (val:make-string "Task 2"))
     (check-equal? (p:get item-state 3 blocks-property) (val:make-set (set (val:make-item 2)))))

   (test-case "Loading saved data gives original result"
     (define st (load-state example-path))
     (check-equal? st (load-state-from-string #:open-file example-path (save-state-to-string st))))

   (test-case "Task being inactive does not override base value of 'status' when saving even if 'status' is set explictly"
     (define st (load-state example-path))
     (define st2 (load-state-from-string #:open-file #f (save-state-to-string st)))
     (define item-state (a:get-path (st2 state.item-state)))
     (check-equal? (i:get item-state 2 'status) (val:make-string "active")))

   (test-case "Property 'order' is loaded correctly even if it comes before 'date'"
     (define st (load-example "example-6.t"))
     (define item-state (a:get-path (st state.item-state)))
     (check-equal? (p:get item-state 1 order-property) (val:make-number 1)))))
