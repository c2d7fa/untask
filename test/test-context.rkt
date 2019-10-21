#lang racket

(require rackunit
         untask/test/util
         untask/src/squiggle

         untask/src/untask/core/context)

(provide context-tests)

(define example-1
  (~> empty-state
      (register "context-1" #:filter '(filter 1) #:modify '(modify 1))
      (register "context-2" #:filter '(filter 2) #:modify '(modify 2))))

(define example-2
  (~> example-1
      (activate "context-1")
      (activate "context-2")))

(define context-tests
  (test-suite "Contexts"
    (test-case "Registering a context makes it available"
      (check-false (available? empty-state "context-1"))
      (check-true (available? (register empty-state "context-1" #:filter '(filter 1) #:modify '(modify 1)) "context-1")))

    (test-case "Getting the filter an modify expression of a context"
      (check-equal? (filter example-1 "context-1") '(filter 1))
      (check-equal? (modify example-1 "context-1") '(modify 1)))

    (test-case "Listing available contexts"
      (check same-set? (available-names example-1) '("context-1" "context-2")))

    (test-case "Removing contexts"
      (check same-set? (~> example-1
                           (remove "context-1")
                           (available-names))
                       '("context-2")))

    (test-case "Activating contexts"
      (check same-set? (~> example-1 (activated-names))
                       '())
      (check same-set? (~> example-1
                           (activate "context-1")
                           (activated-names))
                       '("context-1"))
      (check same-set? (~> example-1
                           (activate "context-1")
                           (activate "context-2")
                           (activated-names))
                       '("context-1" "context-2")))

    (test-case "Deactivating contexts"
      (check same-set? (~> example-2 (activated-names)) '("context-1" "context-2"))
      (check same-set? (~> example-2 (deactivate "context-1") (activated-names)) '("context-2")))

    (test-case "Deactivating all contexts"
      (check same-set? (~> example-2 (activated-names)) '("context-1" "context-2"))
      (check same-set? (~> example-2 (deactivate-all) (activated-names)) '()))

    (test-case "Activated contexts filter expression"
      ;; TODO: The filter expression may have a different order, and may omit
      ;; 'and' when there is only one expression -- it does not need to be
      ;; equal!
      (check-equal? (activated-filter example-2) '(and (filter 1) (filter 2)))
      (check-equal? (activated-filter (~> example-2 (deactivate "context-1"))) '(and (filter 2))))

    (test-case "Activated contexts modify expression"
      ;; TODO: See above.
      (check-equal? (activated-modify example-2) '(and (modify 1) (modify 2))))

    (test-case "Toggling contexts"
      (define s example-2)
      (check same-set? (activated-names s) '("context-1" "context-2"))
      (set! s (~> s (toggle '((off "context-1")))))
      (check same-set? (activated-names s) '("context-2"))
      (set! s (~> s (toggle '((off "context-2") (on "context-1")))))
      (check same-set? (activated-names s) '("context-1"))
      (set! s (~> s (toggle '((on "context-1") (reset)))))
      (check same-set? (activated-names s) '()))))
