#lang racket

(provide (all-defined-out))

(require
 (prefix-in operators: "operators.rkt")
 (prefix-in data: "item-data.rkt"))

;; Take a modify expression and return a function that will update the
;; given item in the given item-data according to the
;; modify-expression.
(define ((evaluate-modify-expression modify-expression) item-data item)
  (match modify-expression
    (`(,property ,operator ,value) #:when (symbol? operator)
     (if (eq? operator ':)
         (data:set-property item-data item property value)
         (data:set-property item-data item property (operators:evaluate-operator-expression
                                                     operators:common-operators
                                                     (list (data:get-property item-data item property)
                                                           operator
                                                           value)))))
    (`(and ,subexpressions ...)
     (foldl (λ (subexpression item-data)
              ((evaluate-modify-expression subexpression) item-data  item))
            item-data
            subexpressions))))

;; Returns new-item-data after modifying all items in item-data
;; according to modify-expression.
(define (modify-items item-data items modify-expression)
  (foldl (λ (item item-data)
           ((evaluate-modify-expression modify-expression) item-data item))
         item-data
         (set->list items)))
