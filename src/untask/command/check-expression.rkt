#lang racket

(provide check-filter/modify-expression)

(require
 (prefix-in val: untask/src/untask/core/value)
 (prefix-in op: untask/src/untask/core/operator)
 (prefix-in p: untask/src/untask/core/property)
 untask/src/untask/user/builtin-operators
 (prefix-in bp: untask/src/untask/properties/builtin)
 (prefix-in a: untask/src/attribute))

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
    (`(edit)
     (when filter? (error "Error!")) ; NOTE: This cannot be parsed in filter context.
     #t)
    (`(edit ,property)
     (when filter? (error "Error!")) ; NOTE: This cannot be parsed in filter context.
     (if (not (bp:has? property))
         (format "Unknown property '~A'." property)
         (if (equal? 'string (p:type (bp:ref property)))
             #t
             (format "Property '~A' of type '~A' cannot be edited because it is not of type string." property (p:type (bp:ref property))))))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
                                          (if (not (bp:has? property))
                                              (format "Unknown property '~A'." property)
                                              (let ((op (op:operator-definitions-find builtin-operators
                                                                                      operator
                                                                                      (p:type (bp:ref property))
                                                                                      filter?)))
                                                (if (eq? op #f)
                                                    (format "Unknown operator '~A' on property '~A'." operator property)
                                                    (op:check-types op
                                                                    #:object-type (p:type (bp:ref property))
                                                                    #:argument-type (val:get-type (val:evaluate-literal literal-expr)))))))
    (`(item . ,id) #t)
    ('() #t)))
