#lang racket

(require rackunit
         untask/src/untask/command/check-expression)

(provide check-expression-tests)

(define check-expression-tests
  (test-suite "Checking Expressions"
    (test-case "Incorrect operator for color property is reported"
      (check-equal?
       (check-filter/modify-expression '(color + (string . "cyan")) #t)
       "Only operator ':' can be used on property 'color', which has valid values: 'red', 'green', 'yellow', 'blue', 'magenta', 'cyan'."))
    (test-case "Incorrect value for color property is reported"
      (check-equal?
       (check-filter/modify-expression '(color : (string . "white")) #t)
       "Invalid value 'white' for property 'color'. Valid values are: 'red', 'green', 'yellow', 'blue', 'magenta', 'cyan'."))))
