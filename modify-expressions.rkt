#lang racket

(provide (all-defined-out))

(require
 (prefix-in data: "item-data.rkt"))

;; Take a modify expression and return a function that will update the
;; given item in the given item-data according to the
;; modify-expression.
(define ((evaluate-modify-expression modify-expression) item-data item)
  (define ((evaluate-modify-operator operator) property-value argument)
    (case operator
      ((:) argument)
      ((<) (string-append argument property-value))
      ((>) (string-append property-value argument))
      ((+) (set-add property-value argument))
      ((-) (set-remove property-value argument))
      ))
  (match modify-expression
    (`(,property ,operator ,value) #:when (symbol? operator)
     (data:set-property item-data item property ((evaluate-modify-operator operator)
                                                 (data:get-property item-data item property)
                                                 value)))
    (`(and ,subexpressions ...)
     (foldl (λ (subexpression item-data)
              ((evaluate-modify-expression subexpression) item-data  item))
            item-data
            subexpressions))))
