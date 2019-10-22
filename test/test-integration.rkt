#lang racket

(require rackunit
         untask/test/util
         untask/src/untask/user/execute
         untask/src/untask/user/parser
         untask/src/untask/core/state
         (prefix-in i: untask/src/untask/core/item)
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
                   "  1. Test item  0 B:1\n  2. Other test item  0 D:1\n"))

   (test-case "Copying item"
     (define sb (box state-empty))
     (capture-output
       (thunk (execute! (parse "add {Item 1} #tag date:Jan-1") sb)
              (execute! (parse "1 copy urgency+$1 date+$7") sb)))
     (check-equal? (capture-output (thunk (execute! (parse "list") sb)))
                   "  2. Item 1 #tag 1 D:Jan-08\n  1. Item 1 #tag 0 D:Jan-01\n"))

   (test-case "Copying item with recur"
     (define sb (box state-empty))
     (capture-output (thunk (execute! (parse "add {Some item} date:Jan-4 wait:Jan-2") sb)
                            (execute! (parse "add {Another item}") sb)))
     ;; TODO: This is a hack; we copy it from the year 3000, because the output
     ;; of the command depends on the current date.
     (check-equal? (capture-output (thunk (execute! (parse "1 copy urgency+$1 from 3000-Feb-4 to 3000-Feb-20 by 5") sb)))
                   ;; TODO: This output depends on the current date!
                   "  3. Some item  1 W:3000-Feb-02 D:3000-Feb-04\n  4. Some item  1 W:3000-Feb-07 D:3000-Feb-09\n  5. Some item  1 W:3000-Feb-12 D:3000-Feb-14\n  6. Some item  1 W:3000-Feb-17 D:3000-Feb-19\n")
     (check-equal? (capture-output (thunk (execute! (parse "2 copy from 3000-Feb-1 to 3000-Feb-2 by 1") sb)))
                   "  7. Another item  0 D:3000-Feb-01\n  8. Another item  0 D:3000-Feb-02\n"))))
