#lang racket

(require rackunit
         untask/test/util
         untask/src/untask/properties/links

         untask/src/untask/core/state
         (prefix-in i: untask/src/untask/core/item)
         (prefix-in p: untask/src/untask/core/property)
         untask/src/untask/properties/description
         untask/src/untask/properties/dependencies
         (prefix-in a: untask/src/attribute)
         (prefix-in val: untask/src/untask/core/value)
         untask/src/squiggle)

(provide links-tests)

(define links-tests
  (test-suite
   "Links"
   (test-case "Adding a child updates parents"
     (check-equal? (~> i:empty-state
                       (i:new/state)       ; id=1
                       (i:new/state)       ; id=2
                       (p:set 1 children-property (val:make-set (set (val:make-item 2))))
                       (p:get 2 parents-property))
                   (val:make-set (set (val:make-item 1)))))))
