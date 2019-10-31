#lang racket

(provide check-filter/modify-expression)

(require
 (prefix-in val: untask/src/untask/core/value)
 (prefix-in op: untask/src/untask/core/operator)
 (prefix-in p: untask/src/untask/core/property)
 untask/src/untask/user/builtin-operators
 (prefix-in bp: untask/src/untask/properties/builtin)
 (prefix-in a: untask/src/attribute))

;; For enum types, only direct assignment/comparison of an appropriate value is valid.
(define (check-enum property operator literal-expr)
  (define valid-values/symbols
    (cdr (p:type (bp:ref property))))
  (define valid-values/string
    (string-join (map (λ (s) (format "'~A'" s))
                      valid-values/symbols)
                 ", "))
  (if (not (eq? operator ':))
      (format "Only operator ':' can be used on property '~A', which has valid values: ~A." property valid-values/string)
      (let ((symbol-value (string->symbol (val:unwrap-string (val:evaluate-literal literal-expr)))))
        (if (not (member symbol-value valid-values/symbols))
            (format "Invalid value '~A' for property '~A'. Valid values are: ~A." symbol-value property valid-values/string)
            #t))))

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
                               #:argument-value (val:evaluate-literal literal-expr))))))
    (`(item . ,id) #t)
    ('() #t)))
