#lang racket

(provide (all-defined-out))

(require
 (prefix-in operators: "../core/operator.rkt")
 (prefix-in item: "../core/item.rkt")
 (prefix-in val: "../core/value.rkt")
 (prefix-in prop: "../core/property.rkt")

 (only-in "../user/builtin-operators.rkt" builtin-operators)

 (only-in "../../misc.rkt" set-filter)
 )

;; Returns #t if valid filter expression, otherwise returns error string.
(define (check-filter-expression filter-expression #:property-types property-types)
  (define (check subexpr)
    (check-filter-expression subexpr #:property-types property-types))
  (define (collect subexprs)
    (foldl (λ (subexpr total)
             (if (eq? total #t)
                 (check subexpr)
                 total))
           #t
           subexprs))
  (match filter-expression
    (`(and ,subexprs ...)
     (collect subexprs))
    (`(or ,subexprs ...)
     (collect subexprs))
    (`(not ,subexpr)
     (check subexpr))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     ;; TODO: Special handling of ":" operator
     (let ((pr (prop:get-property-type property-types property)))
       (if (eq? pr #f)
           (format "Unknown property '~a'." property)
           (let ((op (operators:operator-definitions-find builtin-operators
                                                          operator
                                                          (prop:property-type-type pr)
                                                          #t)))
             (if (eq? op #f)
                 (format "Unknown operator '~a' on property '~a'." operator property)
                 (if (val:type<=? (val:get-type (val:evaluate-literal literal-expr))
                                  (car (operators:operator-argument-types op)))
                     #t
                     (format "Incorrect argument type for operator '~a' on property '~a'; expected type ~a."
                             operator
                             property
                             (car (operators:operator-argument-types op)))))))))
    (`(item . ,id) #t)
    ('() #t)))

;; Take a filter expression and return a function that returns whether
;; an item matches the filter.
(define (evaluate-filter-expression filter-expression item-data item #:property-types property-types)
  (match filter-expression
    (`(and ,subexprs ...)
     (andmap (λ (subexpr)
               (evaluate-filter-expression subexpr item-data item #:property-types property-types))
             subexprs))
    (`(or ,subexprs ...)
     (ormap (λ (subexpr)
              (evaluate-filter-expression subexpr item-data item #:property-types property-types))
            subexprs))
    (`(not ,subexpr)
     (not (evaluate-filter-expression subexpr item-data item #:property-types property-types)))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     (if (eq? operator ':)
         (equal? (item:get-property-by-key item-data item property #:property-types property-types)
                 (val:evaluate-literal literal-expr))
         (operators:evaluate-operator-expression builtin-operators
                                                 (list (item:get-property-by-key item-data item property #:property-types property-types)
                                                       operator
                                                       (val:evaluate-literal literal-expr))
                                                 #t)))
    (`(item . ,id)
     (= (item:item-id item-data item) id))
    ((list)
     #t)))

;; Returns a set of all items with values in item-data matching
;; filter-expression.
(define (search item-data filter-expression #:property-types property-types)
  (set-filter (λ (item) (evaluate-filter-expression filter-expression item-data item #:property-types property-types))
              (item:all-items item-data)))
