#lang racket

(require rackunit
         "util.rkt"
         "../src/untask/properties/dependencies.rkt"

         "../src/untask/core/state.rkt"
         (prefix-in i: "../src/untask/core/item.rkt")
         (prefix-in p: "../src/untask/core/property.rkt")
         "../src/untask/properties/description.rkt"
         "../src/untask/properties/dependencies.rkt"
         (prefix-in a: "../src/attribute.rkt")
         (prefix-in val: "../src/untask/core/value.rkt")
         "../src/squiggle.rkt")

(provide dependencies-tests)

(define dependencies-tests
  (test-suite
   "Dependencies"
   (test-case "Adding a dependency also sets blocked item"
     (check-equal? (~> i:empty-state
                       (i:new/state)       ; id=1
                       (i:new/state)       ; id=2
                       (p:set 1 depends-property (val:make-set (set (val:make-item 2))))
                       (p:get 2 blocks-property))
                   (val:make-set (set (val:make-item 1)))))))
