#lang racket

(require rackunit
         "../../src/user/parser.rkt")

(define/provide-test-suite parser-tests
  (test-case "add"
   (check-equal? (parse "add description:{Some item} tags+my-tag blocks-3")
                 '(add (and (description : (string . "Some item"))
                            (tags + (string . "my-tag"))
                            (blocks - (item . 3))))))

  (test-case "modify"
   (check-equal? (parse "description<prefix tags+tag, description/search modify depends+1 urgency+$2.5")
                 '((or (and (description < (string . "prefix"))
                            (tags + (string . "tag")))
                       (and (description / (string . "search"))))
                   modify
                   (and (depends + (item . 1))
                        (urgency + (number . 2.5)))))))
