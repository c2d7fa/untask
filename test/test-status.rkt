#lang racket

(require rackunit untask/src/untask/command/check-expression)

(provide status-tests)

(define status-tests
  (test-suite "Status"
    (test-case "Invalid value is reported"
      (check-equal? (check-filter/modify-expression '(status : (string . "invalid")) #t)
                    "Invalid argument type: Expected type (enum active inactive done) but got string."))
    (test-case "Valid value is accepted"
      (check-equal? (check-filter/modify-expression '(status : (string . "active")) #t) #t))))
