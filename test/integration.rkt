#lang racket

(require rackunit
         "./util.rkt")

(define/provide-test-suite integration-tests
  (test-case "filtering based on tags"
    (let* ((state (build-state "add description:{item 1} tags+tag1 tags+tag2"
                               "add description:{item 2} tags+tag1"
                               "add description:{item 3} tags+tag2"))
           (items (run-list-command "tags+tag2" state)))
      (writeln items)
      (check-equal? (get-descriptions items state)
                    (set "item 1" "item 3")))))
