#lang racket

(require rackunit untask/test/util
         untask/src/squiggle
         (prefix-in a: untask/src/attribute)
         (prefix-in dt: untask/src/datetime)

         untask/src/untask/core/state
         (only-in untask/src/untask/properties/order order-property)
         (only-in untask/src/untask/properties/date date-property)
         (prefix-in i: untask/src/untask/core/item)
         (prefix-in p: untask/src/untask/core/property)
         (prefix-in v: untask/src/untask/core/value)
         (prefix-in bp: untask/src/untask/properties/builtin)
         (prefix-in cmd: untask/src/untask/command/command))

(provide order-tests)

(define st1-full (load-example "example-5.t"))
(define st1 (a:get-path (st1-full state.item-state)))

(define order-tests
  (test-suite "Order (in agenda)"
    (test-case "Items have no order set by default"
      (check-equal? (~> i:empty-state
                        (i:new/state)
                        (i:set 1 'description (v:make-string "Test"))
                        (p:get 1 (bp:ref 'order)))
                    #f))

    (test-case "Setting order on an item with no date does nothing"
      (check-equal? (~> i:empty-state
                        (i:new/state)
                        (i:set 1 'description (v:make-string "Test"))
                        (p:set 1 order-property (v:make-number 3))
                        (p:get 1 order-property))
                    #f))

    (test-case "Setting order beyond last sets highest possible"
      (check-equal? (~> st1
                        (p:set 1 order-property (v:make-number 10))
                        (p:get 1 order-property))
                    (v:make-number 4))
      (check-equal? (~> st1
                        (p:set 3 order-property (v:make-number 10))
                        (p:get 3 order-property))
                    (v:make-number 5))
      (check-equal? (~> st1
                        (p:set 5 order-property (v:make-number 10))
                        (p:get 5 order-property))
                    (v:make-number 4)))

    (test-case "Setting same order as existing item nudges other items around"
      (let ((st* (~> st1 (p:set 1 order-property (v:make-number 1)))))
        (check-equal? (p:get st* 1 order-property) (v:make-number 1))
        (check-equal? (p:get st* 5 order-property) (v:make-number 2))
        (check-equal? (p:get st* 2 order-property) (v:make-number 3))
        (check-equal? (p:get st* 4 order-property) (v:make-number 4)))
      (let ((st* (~> st1 (p:set 6 order-property (v:make-number 3)))))
        (check-equal? (p:get st* 5 order-property) (v:make-number 1))
        (check-equal? (p:get st* 2 order-property) (v:make-number 2))
        (check-equal? (p:get st* 6 order-property) (v:make-number 3))
        (check-equal? (p:get st* 4 order-property) (v:make-number 4))
        (check-equal? (p:get st* 1 order-property) (v:make-number 5))))

    (test-case "Resetting an item's order nudges other items"
      (let ((st* (~> st1 (p:set 2 order-property #f))))
        (check-equal? (p:get st* 5 order-property) (v:make-number 1))
        (check-equal? (p:get st* 4 order-property) (v:make-number 2))
        (check-equal? (p:get st* 1 order-property) (v:make-number 3))
        (check-equal? (p:get st* 2 order-property) #f)))

    (test-case "Agenda view with both ordered and unordered items"
      ;; If some items have order and others don't, ordered items are displayed first in agenda view.
      (check-equal? (cmd:agenda st1-full #:filter '())
                    `((,(dt:datetime 2019 11 03) 5 2 4 1 3 6))))

    (test-case "Modifying an item's date resets its order"
      (check-equal? (~> st1
                        (p:set 1 date-property (v:make-date (dt:datetime 2019 11 04)))
                        (p:get 1 order-property))
                    #f))

    (test-case "Modifying and item's date nudges other items"
      (let ((st* (~> st1 (p:set 2 date-property (v:make-date (dt:datetime 2019 11 04))))))
        (check-equal? (p:get st* 5 order-property) (v:make-number 1))
        (check-equal? (p:get st* 4 order-property) (v:make-number 2))
        (check-equal? (p:get st* 1 order-property) (v:make-number 3))
        (check-equal? (p:get st* 2 order-property) #f)))

    (test-case "Copying an ordered item updates order"
      (let*-values (((st*-full _) (cmd:copy st1-full #:filter '(item . 4) #:modify '()))
                    ((st*) (a:get-path (st*-full state.item-state))))
        (check-equal? (p:get st* 5 order-property) (v:make-number 1))
        (check-equal? (p:get st* 2 order-property) (v:make-number 2))
        (check-equal? (p:get st* 7 order-property) (v:make-number 3))  ; New item
        (check-equal? (p:get st* 4 order-property) (v:make-number 4))
        (check-equal? (p:get st* 1 order-property) (v:make-number 5))))

    (test-case "Copying an ordered item to another does not change other items' orders"
      (let*-values (((st*-full _) (~> st1-full (cmd:copy #:filter '(item . 4) #:modify '(date + (number . 1)))))
                    ((st*) (a:get-path (st*-full state.item-state))))
        (check-equal? (p:get st* 5 order-property) (v:make-number 1))
        (check-equal? (p:get st* 2 order-property) (v:make-number 2))
        (check-equal? (p:get st* 4 order-property) (v:make-number 3))
        (check-equal? (p:get st* 1 order-property) (v:make-number 4))))))
