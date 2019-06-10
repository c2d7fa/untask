#lang racket

(provide (all-defined-out))

(require
 (prefix-in operators: "../core/operator.rkt")
 (prefix-in item: "../core/item.rkt")
 (prefix-in val: "../core/value.rkt")
 (prefix-in prop: "../core/property.rkt")
 
 (only-in "../user/builtin-operators.rkt" builtin-operators))

;; Returns #t if valid modify expression, otherwise returns error string.
(define (check-modify-expression modify-expression #:property-types property-types)
  (define (check subexpr)
    (check-modify-expression subexpr #:property-types property-types))
  (define (collect subexprs)
    (foldl (λ (subexpr total)
             (if (eq? total #t)
                 (check subexpr)
                 total))
           #t
           subexprs))
  (match modify-expression
    (`(and ,subexprs ...)
     (collect subexprs))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     (let ((pr (prop:get-property-type property-types property)))
       (if (eq? pr #f)
           (format "Unknown property '~a'." property)
           (if (eq? ': operator)
               (if (val:type<=? (val:get-type (val:evaluate-literal literal-expr))
                                (prop:property-type-type pr))
                   #t
                   (format "Incorrect argument type for operator '~a' on property '~a'; expected type ~a"
                           operator
                           property
                           (prop:property-type-type pr)))
               (let ((op (operators:operator-definitions-find builtin-operators
                                                              operator
                                                              (prop:property-type-type pr)
                                                              #f)))
                 (if (eq? op #f)
                     (format "Unknown operator '~a' on property '~a'." operator property)
                     (if (val:type<=? (val:get-type (val:evaluate-literal literal-expr))
                                      (car (operators:operator-argument-types op)))
                         #t
                         (format "Incorrect argument type for operator '~a' on property '~a'; expected type ~a."
                                 operator
                                 property
                                 (car (operators:operator-argument-types op))))))))))
    ('() #t)))

;; Take a modify expression and return a function that will update the
;; given item in the given item-data according to the
;; modify-expression.
(define (evaluate-modify-expression modify-expression item-data item #:property-types property-types)
  (match modify-expression
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     (if (eq? operator ':)
         (item:set-property-by-key #:property-types property-types
                                   item-data item property (val:evaluate-literal literal-expr))
         (item:set-property-by-key #:property-types property-types
                                   item-data item property (operators:evaluate-operator-expression
                                                            builtin-operators
                                                            (list (item:get-property-by-key item-data item property #:property-types property-types)
                                                                  operator
                                                                  (val:evaluate-literal literal-expr))))))
    (`(and ,subexpressions ...)
     (foldl (λ (subexpression item-data)
              (evaluate-modify-expression subexpression item-data item #:property-types property-types))
            item-data
            subexpressions))
    ((list)
     item-data)))

;; Returns new-item-data after modifying all items in item-data according to
;; modify-expression.
(define (modify-items item-data items modify-expression #:property-types property-types)
  (foldl (λ (item item-data)
           (evaluate-modify-expression modify-expression item-data item #:property-types property-types))
         item-data
         (set->list items)))
