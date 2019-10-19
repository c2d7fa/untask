#lang racket

(require rackunit

         "../src/untask/user/execute.rkt"
         "../src/untask/user/parser.rkt"
         "../src/untask/core/state.rkt"
         (prefix-in i: "../src/untask/core/item.rkt")
         "../src/untask/properties/description.rkt"
         (prefix-in a: "../src/attribute.rkt")
         (prefix-in val: "../src/untask/core/value.rkt")

         "util.rkt"
         "../src/squiggle.rkt")

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
                   "  1. Test item  0 B:1\n  2. Other test item  0 D:1\n"))
   (test-case "Copying item"
     (define sb (box state-empty))
     (void (execute! (parse "add {Item 1} #tag date:Jan-1") sb)
           (execute! (parse "1 copy urgency+$1 date+$7") sb))
     (check-equal? (capture-output (thunk (execute! (parse "list") sb)))
                   "  2. Item 1 #tag 1 D:Jan-08\n  1. Item 1 #tag 0 D:Jan-01\n"))))
