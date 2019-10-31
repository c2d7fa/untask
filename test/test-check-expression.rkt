#lang racket

(require rackunit
         untask/src/untask/command/check-expression)

(provide check-expression-tests)

(define check-expression-tests
  (test-suite "Checking Expressions"
    (test-suite "Optional enums"
      (test-case "Incorrect operator for color property is reported"
        (check-equal?
         (check-filter/modify-expression '(color + (string . "cyan")) #t)
         "Unknown operator '+' on property 'color'."))

      (test-case "Incorrect value for color property is reported"
        (check-equal?
         (check-filter/modify-expression '(color : (string . "white")) #t)
         "Invalid argument type: Expected type (opt (enum red green yellow blue magenta cyan)) but got string."))

      (test-case "Can set color to empty"
        (check-equal? (check-filter/modify-expression '(color : #f) #f) #t))

      (test-case "Valid value for color property is accepted"
        (check-equal? (check-filter/modify-expression '(color : (string . "magenta")) #f) #t)))))
