#lang racket

(require rackunit
         untask/test/util

         untask/src/untask/core/state
         (prefix-in i: untask/src/untask/core/item)
         (prefix-in v: untask/src/untask/core/value)
         (prefix-in bp: untask/src/untask/properties/builtin)
         (prefix-in cmd: untask/src/untask/command/command)
         (prefix-in a: untask/src/attribute)
         untask/src/squiggle)

(provide urgency-tests)

(define urgency-tests
  (test-suite "Urgency"
    (test-case "Updating calculated urgency sets the base urgency correctly"
      (let ((st (load-example "example-8.t")))
        (define item-state (a:get-path (st state.item-state)))
        (check-equal? (i:get item-state 1 'urgency) (v:make-number 1))
        (check-equal? (bp:get item-state 1 'urgency) (v:make-number 4))
        (define-values (st* _) (cmd:modify st #:filter '(item . 1) #:modify '(urgency + (number . 2))))
        (define item-state* (a:get-path (st* state.item-state)))
        (check-equal? (i:get item-state* 1 'urgency) (v:make-number 3))
        (check-equal? (bp:get item-state* 1 'urgency) (v:make-number 6))))))
