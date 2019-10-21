#lang racket

(require rackunit
         untask/test/util
         untask/src/untask/user/execute
         untask/src/untask/user/parser
         untask/src/untask/core/state
         (prefix-in item: untask/src/untask/core/item)
         untask/src/untask/properties/description
         (prefix-in a: untask/src/attribute)
         (prefix-in val: untask/src/untask/core/value))

(provide integration-tests)

(define integration-tests
  (test-suite
   "Integration"
   (test-case "Listing items with dependency"
     (define state-box (box state-empty))
     (check-equal? (capture-output (thunk (execute! (parse "add {Test item}") state-box)))
                   "Test item\n\nID:      1\nStatus:  active\nUrgency: 0\nTags:    \n")
     (check-equal? (capture-output (thunk (execute! (parse "add {Other test item} depends+1") state-box)))
                   "Other test item\n\nID:      2\nStatus:  inactive\nUrgency: 0\nTags:    \n\nDepends on:\n      1. Test item  0 B:1\n")
     (check-equal? (capture-output (thunk (execute! (parse "list") state-box)))
                   "  1. Test item  0 B:1\n  2. Other test item  0 D:1\n"))))