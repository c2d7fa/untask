#lang racket

(provide (all-defined-out))

(require
 (only-in "../util.rkt" set-filter)

 (prefix-in operators: "operators.rkt")

 (prefix-in data: "item-data.rkt"))

;; Take a filter expression and return a function that returns whether
;; an item matches the filter.
(define ((evaluate-filter-expression filter-expression) item-data item)
  (match filter-expression
    (`(and ,subexprs ...)
     (andmap (λ (subexpr)
               ((evaluate-filter-expression subexpr) item-data item))
             subexprs))
    (`(or ,subexprs ...)
     (ormap (λ (subexpr)
              ((evaluate-filter-expression subexpr) item-data item))
            subexprs))
    (`(not ,subexpr)
     (not ((evaluate-filter-expression subexpr) item-data item)))
    (`(,property ,operator ,value) #:when (symbol? operator)
     (if (eq? operator ':)
         (equal? (data:get-property item-data item property) value)
         (operators:evaluate-operator-expression operators:common-operators
                                                 (list (data:get-property item-data item property)
                                                       operator
                                                       value)
                                                 #t)))
    (id #:when (integer? id)
     (= (data:item-id item-data item) id))))

;; Returns a set of all items with values in item-data matching
;; filter-expression.
(define (search item-data filter-expression)
  (set-filter (λ (item) ((evaluate-filter-expression filter-expression) item-data item))
              (data:all-items item-data)))
