#lang racket

(require rackunit

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
                      (date 2019 01 01)
                      (date 2019 01 10)
                      2)))

    (test-case "Copy command with recurrence without modify expression"
      (check-equal? (parse "1 copy from Feb-1 to Feb-20 by 1")
                    '((or (and (item . 1)))
                      copy-recur
                      (and)
                      (date 2019 02 01)
                      (date 2019 02 20)
                      1)))

    (test-case "Parsing command with 'Today' and 'Today+7'"
      (check-equal? (parse "14 modify wait:Today date:Today+7")
                    '((or (and (item . 14)))
                      modify
                      (and (wait : (date today 0))
                           (date : (date today 7))))))))
