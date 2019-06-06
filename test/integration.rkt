#lang racket

(require rackunit
         "./util.rkt")

(define/provide-test-suite integration-tests
  (test-case "filtering based on tags"
    (let* ((state (build-state "add description:{item 1} tags+tag1 tags+tag2"
                               "add description:{item 2} tags+tag1"
                               "add description:{item 3} tags+tag2"))
           (items (run-list-command "tags+tag2" state)))
      (check-equal? (get-descriptions items state)
                    (set "item 1" "item 3"))))

  (test-case "adding and activating single context"
    (let* ((state (build-state "add description:{item 1} status:inactive"
                               "add description:{item 2}"
                               "add description:{item 3} status:done"
                               "add description:{item 4} status:inactive tags+tagged"
                               "add description:{item 5} status:done"
                               "context add unfinished filter !status:done"
                               "@unfinished"))
           (all-items (run-list-command "" state))
           (untagged-items (run-list-command "tags-tagged" state)))
      (check-equal? (get-descriptions all-items state)
                    (set "item 1" "item 2" "item 4"))
      (check-equal? (get-descriptions untagged-items state)
                    (set "item 1" "item 2"))))
  )
