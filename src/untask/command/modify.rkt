#lang racket

(provide (all-defined-out))

(require
 (prefix-in operators: "../core/operator.rkt")
 (prefix-in item: "../core/item.rkt")
 (prefix-in val: "../core/value.rkt")

 "../user/builtin-operators.rkt"
 "../properties/builtin.rkt")

;; Take a modify expression and return a function that will update the
;; given item in the given item-data according to the
;; modify-expression.
(define (evaluate-modify-expression modify-expression item-data item)
  (match modify-expression
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     (set-property-by-key item-data item property (operators:evaluate-operator-expression
                                                   builtin-operators
                                                   #:object (get-property-by-key item-data item property)
                                                   #:operator operator
                                                   #:argument (val:evaluate-literal literal-expr)
                                                   #:object-type (get-property-type-type property)
                                                   #:filter? #f)))
    (`(and ,subexpressions ...)
     (foldl (λ (subexpression item-data)
              (evaluate-modify-expression subexpression item-data item))
            item-data
            subexpressions))
    ((list)
     item-data)))

;; Returns new-item-data after modifying all items in item-data according to
;; modify-expression.
(define (modify-items item-data items modify-expression)
  (foldl (λ (item item-data)
           (evaluate-modify-expression modify-expression item-data item))
         item-data
         (set->list items)))
