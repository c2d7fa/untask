#lang racket

;; todo: we should allow modules to register their properties with
;; value. we should also support read-only properties in this way.
;;
;; (register-property 'base-urgency #:type 'number)
;; (register-calulated-property 'urgency #:calculate compute-urgency)

(require
 (prefix-in data: "item-data.rkt")

 (only-in "util.rkt" set-filter)

 ; Properties
 (prefix-in description: "description.rkt")
 (prefix-in status: "status.rkt")
 (prefix-in tags: "tags.rkt")
 (prefix-in urgency: "urgency.rkt"))

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

;; Take a filter expression and return a function that returns whether
;; an item matches the filter.
(define ((evaluate-filter-expression filter-expression) item-data item)
  (define ((evaluate-filter-operator operator) property-value argument)
    (case operator
      ((:) (equal? property-value argument))
      ((/) (string-contains? property-value argument))
      ((<) (string-prefix? property-value argument))
      ((>) (string-suffix? property-value argument))
      ((+) (set-member? property-value argument))
      ((-) (not (set-member? property-value argument)))
      ))
  (match filter-expression
    (`(and ,subexprs ...)
     (andmap (λ (subexpr)
               ((evaluate-filter-expression subexpr) item-data item))
             subexprs))
    (`(or ,subexprs ...)
     (ormap (λ (subexpr)
              ((evaluate-filter-expression subexpr) item-data item))
            subexprs))
    (`(not ,subexpr)
     (not ((evaluate-filter-expression subexpr) item-data item)))
    (`(,property ,operator ,value) #:when (symbol? operator)
     ((evaluate-filter-operator operator) (data:get-property item-data item property)
                                          value))))

;; Returns a set of all items with values in item-data matching
;; filter-expression.
(define (search item-data filter-expression)
  (set-filter (λ (item) ((evaluate-filter-expression filter-expression) item-data item))
              (data:all-items item-data)))

;; EXAMPLE

(define example-item-data
  (let ((register-property-s
         (list status:register-property-base-status
               tags:register-property-tags
               description:register-property-description
               urgency:register-property-base-urgency)))
    (foldl
     (λ (register-property item-data)
       (let-values (((new-item-data _) (register-property item-data)))
         new-item-data))
     data:item-data-empty
     register-property-s)))

(set! example-item-data
      ((evaluate-modify-expression '(and (description : "tagged a, b")
                                         (tags + "a")
                                         (tags + "b")))
       example-item-data
       0))
(set! example-item-data
      ((evaluate-modify-expression '(and (description : "tagged a")
                                         (tags + "a")))
       example-item-data
       1))
(set! example-item-data
      ((evaluate-modify-expression '(and (description : "tagged b")
                                         (tags + "b")))
       example-item-data
       2))

(search example-item-data
        '(and (description / "tag")
              (not (or (description / ",")
                       (tags - "b")))))
