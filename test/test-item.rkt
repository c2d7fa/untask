#lang racket

(require rackunit
         untask/src/squiggle

         untask/src/untask/core/item)

(provide item-tests)

;; Return #t if the lists represent the same set, #f otherwise.
(define (same-set? l m)
  (equal? (list->set l) (list->set m)))

(define example-1
  (~> empty-state
      (new/state)  ; ID = 1
      (new/state)  ; ID = 2
      (set 1 'description "Item 1")
      (set 2 'description "Item 2")
      (set 1 'urgency 2)
      (set 1 'tags '("tag-1" "tag-2"))
      (set 2 'tags '("tag-1"))))

(define item-tests
  (test-suite "Items"
    (test-check "No items in empty state" same-set? '() (items empty-state))

    (test-case "Setting properties of new item makes it appear in items"
      (define st (~> empty-state
                     (new/state)    ; ID = 1
                     (new/state)))  ; ID = 2
      (check same-set? '() (items st))
      (define st* (~> st (set 2 'description "Item 2")))
      (check same-set? '(2) (items st*))
      (define st** (~> st* (set 1 'description "Item 1")))
      (check same-set? '(1 2) (items st**)))

    (test-case "Getting item properties"
      (check-false (get empty-state 1 'description))
      (check-equal? (get example-1 1 'description) "Item 1")
      (check-equal? (get example-1 1 'tags) '("tag-1" "tag-2")))

    (test-case "Removing item"
      (check same-set? (items example-1) '(1 2))
      (check same-set? (items (remove example-1 1)) '(2)))

    (test-case "Copying item"
      (check-equal? (~> example-1 (get 1 'tags)) '("tag-1" "tag-2"))
      (check-equal? (~> example-1 (copy 2 1) (get 1 'tags)) '("tag-1"))
      (check-equal? (~> example-1 (get 2 'urgency)) #f)
      (check-equal? (~> example-1 (copy 2 1) (copy 1 2) (get 2 'urgency)) 2))

    (test-case "Updating property"
      (check-equal? (get example-1 1 'urgency) 2)
      (check-equal? (get (~> example-1 (update 1 'urgency add1)) 1 'urgency) 3))))
