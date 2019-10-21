#lang racket/base

(require rackunit

         "../src/untask/core/item.rkt"

         "../src/squiggle.rkt")

(provide item-tests)

(define st1 (~> empty-state
                (new/state)             ; id=1
                (new/state)             ; id=2
                (new/state)             ; id=3
                (set 2 'description "Item 2")
                (set 2 'tags '("tag1" "tag2"))
                ))

(define item-tests
  (test-suite
   "Module 'item'"
   (test-suite
    "Procedure 'found?'"
    (test-false "Nonexistent item is not found" (found? st1 3))
    (test-false "Empty item is not found" (found? st1 1))
    (test-true "Existing item is found" (found? st1 2)))
   (let-values (((st* item*) (clone st1 2)))
     (test-suite
      "Procedure 'clone'"
      (test-equal? "New item has expected ID" item* 4)
      (test-case "New item has same properties as old"
        (check-equal? (get st* 4 'description) "Item 2")
        (check-equal? (get st* 4 'tags) '("tag1" "tag2")))))
   ))
