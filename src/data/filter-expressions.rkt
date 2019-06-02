#lang racket

(provide (all-defined-out))

(require
 (only-in "../util.rkt" set-filter)

 (prefix-in operators: "operators.rkt")
 (prefix-in data: "item-data.rkt")
 (prefix-in val: "values.rkt")
 (only-in "../user/builtin-operators.rkt" builtin-operators)
 )

;; Take a filter expression and return a function that returns whether
;; an item matches the filter.
(define ((evaluate-filter-expression filter-expression #:property-types property-types) item-data item)
  (match filter-expression
    (`(and ,subexprs ...)
     (andmap (λ (subexpr)
               ((evaluate-filter-expression subexpr #:property-types property-types) item-data item))
             subexprs))
    (`(or ,subexprs ...)
     (ormap (λ (subexpr)
              ((evaluate-filter-expression subexpr #:property-types property-types) item-data item))
            subexprs))
    (`(not ,subexpr)
     (not ((evaluate-filter-expression subexpr #:property-types property-types) item-data item)))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     (if (eq? operator ':)
         (equal? (data:get-property-by-key item-data item property #:property-types property-types)
                 (val:evaluate-literal literal-expr))
         (operators:evaluate-operator-expression builtin-operators
                                                 (list (data:get-property-by-key item-data item property #:property-types property-types)
                                                       operator
                                                       (val:evaluate-literal literal-expr))
                                                 #t)))
    (`(item . ,id)
     (= (data:item-id item-data item) id))
    ((list)
     #t)))

;; Returns a set of all items with values in item-data matching
;; filter-expression.
(define (search item-data filter-expression #:property-types property-types)
  (set-filter (λ (item) ((evaluate-filter-expression filter-expression #:property-types property-types) item-data item))
              (data:all-items item-data)))
