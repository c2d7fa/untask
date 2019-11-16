#lang racket

(require rackunit
         untask/test/util
         untask/src/untask/properties/dependencies

         untask/src/untask/core/state
         (prefix-in i: untask/src/untask/core/item)
         (prefix-in p: untask/src/untask/core/property)
         untask/src/untask/properties/description
         untask/src/untask/properties/dependencies
         (prefix-in a: untask/src/attribute)
         (prefix-in val: untask/src/untask/core/value)
         untask/src/squiggle)

(provide dependencies-tests)

(define dependencies-tests
  (test-suite
   "Dependencies"
   (test-case "Adding a dependency also updates blocked items"
     (check-equal? (~> i:empty-state
                       (i:new/state)       ; id=1
                       (i:new/state)       ; id=2
                       (p:set 1 depends-property (val:make-set (set (val:make-item 2))))
                       (p:get 2 blocks-property))
                   (val:make-set (set (val:make-item 1)))))

   (test-case "Adding a blocked item also updates dependencies"
     (check-equal? (~> i:empty-state
                       (i:new/state)       ; id=1
                       (i:new/state)       ; id=2
                       (p:set 1 blocks-property (val:make-set (set (val:make-item 2))))
                       (p:get 2 depends-property))
                   (val:make-set (set (val:make-item 1)))))

   (test-case "Removing a blocked item also updates dependencies"
     (define st1 (~> i:empty-state
                     (i:new/state)       ; id=1
                     (i:new/state)       ; id=2
                     (p:set 1 depends-property (val:make-set (set (val:make-item 2))))))
     (check-equal? (p:get st1 2 blocks-property) (val:make-set (set (val:make-item 1))))
     (define st2 (~> st1 (p:set 2 blocks-property (val:make-set (set)))))
     (check-equal? (p:get st2 2 blocks-property) (val:make-set (set)))
     (check-equal? (p:get st2 1 depends-property) (val:make-set (set))))))
