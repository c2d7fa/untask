#lang racket

(provide (all-defined-out))

(require
 (only-in "util.rkt" set-filter)

 (prefix-in data: "item-data.rkt"))

;; Take a filter expression and return a function that returns whether
;; an item matches the filter.
(define ((evaluate-filter-expression filter-expression) item-data item)
  (define ((evaluate-filter-operator operator) property-value argument)
    (case operator
      ((:) (equal? property-value argument))
      ((/) (string-contains? property-value argument))
      ((<) (string-prefix? property-value argument))
      ((>) (string-suffix? property-value argument))
      ((+) (set-member? property-value argument))
      ((-) (not (set-member? property-value argument)))
      ))
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
     ((evaluate-filter-operator operator) (data:get-property item-data item property)
                                          value))))

;; Returns a set of all items with values in item-data matching
;; filter-expression.
(define (search item-data filter-expression)
  (set-filter (λ (item) ((evaluate-filter-expression filter-expression) item-data item))
              (data:all-items item-data)))
