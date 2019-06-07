#lang racket

(provide (all-defined-out))

(require
 (prefix-in operators: "../core/operator.rkt")
 (prefix-in item: "../core/item.rkt")
 (prefix-in val: "../core/value.rkt")

 (only-in "../user/builtin-operators.rkt" builtin-operators))

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
