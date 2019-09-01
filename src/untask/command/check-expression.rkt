#lang racket

(provide check-filter/modify-expression)

(require
 (prefix-in prop: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt")
 (prefix-in op: "../core/operator.rkt")

 "../user/builtin-operators.rkt"
 "../properties/builtin.rkt")

;; Returns #t if expression is valid, otherwise returns human-readable string
;; representing error.
(define (check-filter/modify-expression fm-expression filter?)
  (define (check subexpr)
    (check-filter/modify-expression subexpr filter?))
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
     (let ((pr (prop:get-property-type builtin-property-types property)))
       (if (eq? pr #f)
           (format "Unknown property '~a'." property)
           (let ((op (op:operator-definitions-find builtin-operators
                                                   operator
                                                   (prop:property-type-type pr)
                                                   filter?)))
             (if (eq? op #f)
                 (format "Unknown operator '~a' on property '~a'." operator property)
                 (op:check-types op
                                 #:object-type (prop:property-type-type pr)
                                 #:argument-type (val:get-type (val:evaluate-literal literal-expr))))))))
    (`(item . ,id) #t)
    ('() #t)))
