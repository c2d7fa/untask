#lang racket

(provide evaluate-filter-expression
         search)

(require
 (prefix-in operators: untask/src/untask/core/operator)
 (prefix-in i: untask/src/untask/core/item)
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in val: untask/src/untask/core/value)
 (prefix-in bp: untask/src/untask/properties/builtin)
 untask/src/untask/user/builtin-operators
 (only-in untask/src/misc set-filter))

;; Take a filter expression and return a function that returns whether
;; an item matches the filter.
(define (evaluate-filter-expression filter-expression item-state item)
  (match filter-expression
    (`(and ,subexprs ...)
     (andmap (λ (subexpr)
               (evaluate-filter-expression subexpr item-state item))
             subexprs))
    (`(or ,subexprs ...)
     (ormap (λ (subexpr)
              (evaluate-filter-expression subexpr item-state item))
            subexprs))
    (`(not ,subexpr)
     (not (evaluate-filter-expression subexpr item-state item)))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     (operators:evaluate-operator-expression builtin-operators
                                             #:object (p:get item-state item (bp:ref property))
                                             #:operator operator
                                             #:argument (val:evaluate-literal literal-expr)
                                             #:object-type (p:type (bp:ref property))
                                             #:filter? #t))
    (`(item . ,id)
     (= item id))
    ((list)
     #t)))

;; Returns a set of all items with values in item state matching
;; filter-expression.
(define (search item-state filter-expression)
  (set-filter (λ (item) (evaluate-filter-expression filter-expression item-state item))
              (i:items item-state)))
