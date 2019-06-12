#lang racket

(provide check-filter/modify-expression)

(require
 (prefix-in prop: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt")
 (prefix-in op: "../core/operator.rkt")
 (only-in "../user/builtin-operators.rkt" builtin-operators))


;; Returns #t if expression is valid, otherwise returns human-readable string
;; representing error.
(define (check-filter/modify-expression fm-expression filter? #:property-types property-types)
  (define (check subexpr)
    (check-filter/modify-expression subexpr filter? #:property-types property-types))
  (define (collect subexprs)
    (foldl (λ (subexpr total)
             (if (eq? total #t)
                 (check subexpr)
                 total))
           #t
           subexprs))
  (match fm-expression
    (`(and ,subexprs ...)
     (collect subexprs))
    (`(or ,subexprs ...)
     (collect subexprs))
    (`(not ,subexpr)
     (check subexpr))
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
               (let ((op (op:operator-definitions-find builtin-operators
                                                       operator
                                                       (prop:property-type-type pr)
                                                       filter?)))
                 (if (eq? op #f)
                     (format "Unknown operator '~a' on property '~a'." operator property)
                     (op:check-types op
                                     #:object-type (prop:property-type-type pr)
                                     #:argument-types (list (val:get-type (val:evaluate-literal literal-expr))))))))))
    (`(item . ,id) #t)
    ('() #t)))
