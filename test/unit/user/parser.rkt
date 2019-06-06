#lang racket

(require rackunit
         "../../../src/user/parser.rkt")

(define/provide-test-suite parser-tests
  (test-equal? "add"
               (parse "add description:{Some item} tags+my-tag blocks-3")
               '(add (and (description : (string . "Some item"))
                          (tags + (string . "my-tag"))
                          (blocks - (item . 3)))))
  (test-equal? "modify"
               (parse "description<prefix tags+tag, description/search modify depends+1 urgency+$2.5")
               '((or (and (description < (string . "prefix"))
                          (tags + (string . "tag")))
                     (and (description / (string . "search"))))
                 modify
                 (and (depends + (item . 1))
                      (urgency + (number . 2.5)))))
  (test-exn "invalid input" exn?
            (thunk (parse "description:something, list")))
  (test-equal? "context add"
               (parse "context add my-context modify tags+some-tag")
               '(context add "my-context" () (and (tags + (string . "some-tag")))))
  (test-exn "invalid context add" exn?
            (thunk (parse "context add description/string, tags+tag")))
  (test-equal? "activate/deactivate contexts"
               (parse "@on -@off @other-on @yet-another-on -@some-other-off")
               '(context active ((on "on") (off "off") (on "other-on") (on "yet-another-on") (off "some-other-off"))))
  )
