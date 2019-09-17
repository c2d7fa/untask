#lang racket

(provide check-filter/modify-expression)

(require
 (prefix-in val: "../core/value.rkt")
 (prefix-in op: "../core/operator.rkt")
 "../core/property.rkt"
 "../user/builtin-operators.rkt"
 "../properties/builtin.rkt"
 (prefix-in a: "../../attribute.rkt"))

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
    (`(edit ,property)
     (when filter? (error "Error!")) ; NOTE: This cannot be parsed in filter context.
     (let ((pr (get-property-type builtin-property-types property)))
       (if (eq? pr #f)
           (format "Unknown property '~a'." property)
           (if (equal? 'string (a:get (pr property-type.type)))
               #t
               (format "Property '~a' of type '~a' cannot be edited because it is not of type string." property (a:get (pr property-type.type)))))))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     (let ((pr (get-property-type builtin-property-types property)))
       (if (eq? pr #f)
           (format "Unknown property '~a'." property)
           (let ((op (op:operator-definitions-find builtin-operators
                                                   operator
                                                   (a:get (pr property-type.type))
                                                   filter?)))
             (if (eq? op #f)
                 (format "Unknown operator '~a' on property '~a'." operator property)
                 (op:check-types op
                                 #:object-type (a:get (pr property-type.type))
                                 #:argument-type (val:get-type (val:evaluate-literal literal-expr))))))))
    (`(item . ,id) #t)
    ('() #t)))
