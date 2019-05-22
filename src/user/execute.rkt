#lang racket

(provide (all-defined-out))

(require
 (prefix-in item: "../data/item-data.rkt")
 (prefix-in urgency: "../properties/urgency.rkt")

 "../data/filter-expressions.rkt"
 "../data/modify-expressions.rkt")

;; Convert a representation of a user-inputted command-line into a
;; function that takes the current item-data state and returns
;; new-item-data and an output description (currently just a list of
;; items to print on the screen).
;;
;; command-line-representation is a representation of the parsed user
;; input in the form (filter-expression command-name arguments...)
(define ((execute command-line-representation #:property-types property-types) item-data)
  (match command-line-representation
    (`(,filter-expression list) (values item-data (urgency:sort-items-by-urgency-descending
                                                   item-data
                                                   (search item-data filter-expression #:property-types property-types))))
    (`(add ,modify-expression)
     (let-values (((new-item-data-1 new-item) (item:new-item item-data)))
       (values ((evaluate-modify-expression modify-expression #:property-types property-types) new-item-data-1 new-item)
               (list new-item))))
    (`(,filter-expression modify ,modify-expression)
     (values (modify-items item-data (search item-data filter-expression #:property-types property-types) modify-expression #:property-types property-types)
             (set->list (search item-data filter-expression #:property-types property-types))))))

;; A version of execute with its parameter order and return values
;; modified to be better suited to constructing examples for testing.
(define (execute* command-line-representation item-data #:property-types property-types)
  (let-values (((new-item-data output) ((execute command-line-representation #:property-types property-types) item-data)))
    new-item-data))
