#lang racket

(require rackunit
         untask/src/squiggle

         (only-in untask/src/untask/user/parser parse))

(provide parser-tests)

(define parser-tests
  (test-suite "Parser"
    (test-case "Basic copy command"
      (check-equal? (parse "1, 2 copy status:done")
                    '((or (and (item . 1))
                          (and (item . 2)))
                      copy
                      (and (status : (string . "done"))))))

    (test-case "Copy command with recurrence"
      (check-equal? (parse "1, 2 copy status:done from Jan-1 to Jan-10 by 2")
                    '((or (and (item . 1))
                          (and (item . 2)))
                      copy-recur
                      (and (status : (string . "done")))
                      (2019 01 01)
                      (2019 01 10)
                      2)))))
