#lang racket

(provide (all-defined-out))

(require
 (prefix-in operators: "../core/operator.rkt")
 (prefix-in item: "../core/item.rkt")
 (prefix-in val: "../core/value.rkt")

 "../user/builtin-operators.rkt"
 "../properties/builtin.rkt"

 (only-in "../../misc.rkt" set-filter)
 )

;; Take a filter expression and return a function that returns whether
;; an item matches the filter.
(define (evaluate-filter-expression filter-expression item-data item)
  (match filter-expression
    (`(and ,subexprs ...)
     (andmap (λ (subexpr)
               (evaluate-filter-expression subexpr item-data item))
             subexprs))
    (`(or ,subexprs ...)
     (ormap (λ (subexpr)
              (evaluate-filter-expression subexpr item-data item))
            subexprs))
    (`(not ,subexpr)
     (not (evaluate-filter-expression subexpr item-data item)))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     (if (eq? operator ':)
         (equal? (get-property-by-key item-data item property)
                 (val:evaluate-literal literal-expr))
         (operators:evaluate-operator-expression builtin-operators
                                                 (list (get-property-by-key item-data item property)
                                                       operator
                                                       (val:evaluate-literal literal-expr))
                                                 #t)))
    (`(item . ,id)
     (= (item:item-id item-data item) id))
    ((list)
     #t)))

;; Returns a set of all items with values in item-data matching
;; filter-expression.
(define (search item-data filter-expression)
  (set-filter (λ (item) (evaluate-filter-expression filter-expression item-data item))
              (item:all-items item-data)))
