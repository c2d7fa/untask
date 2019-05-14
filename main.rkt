#lang racket

(require
 (prefix-in data: "item-data.rkt")

 (only-in "util.rkt" thread)

 "filter-expressions.rkt"
 "modify-expressions.rkt"

 (prefix-in status: "status.rkt")
 (prefix-in description: "description.rkt")
 (prefix-in tags: "tags.rkt")
 (prefix-in urgency: "urgency.rkt"))

(define item-data-empty-with-properties
  (thread data:item-data-empty
    (status:register-property-base-status)
    (description:register-property-description)
    (tags:register-property-tags)
    (urgency:register-property-base-urgency)))

;; Convert a representation of a user-inputted command-line into a
;; function that takes the current item-data state and returns
;; new-item-data and an output description (currently just a list of
;; items to print on the screen).
;;
;; command-line-representation is a representation of the parsed user
;; input in the form (filter-expression command-name arguments...)
(define ((execute command-line-representation) item-data)
  (match command-line-representation
    (`(,filter-expression list) (values item-data (set->list (search item-data filter-expression))))
    (`(add ,modify-expression)
     (let-values (((new-item-data-1 new-item) (data:new-item item-data)))
       (values ((evaluate-modify-expression modify-expression) new-item-data-1 new-item)
               (list new-item))))
    (`(,filter-expression modify ,modify-expression)
     (values (modify-items item-data (search item-data filter-expression) modify-expression)
             (search item-data filter-expression)))))

;; A version of execute with its parameter order and return values
;; modified to be better suited to constructing examples for testing.
(define (execute* command-line-representation item-data)
  (let-values (((new-item-data output) ((execute command-line-representation) item-data)))
    new-item-data))

;;; EXAMPLE

(define example-item-data
  (thread item-data-empty-with-properties
    (execute* '(add (and (description : "this is a brand new item") (tags + "some-tag") (tags + "another-tag"))))
    (execute* '(add (and (description : "here is another item") (tags + "another-tag"))))
    (execute* '(add (and (description : "third item") (tags + "some-tag") (tags + "yet-another-tag"))))
    (execute* '((not (description / "third")) modify (and (tags - "some-tag") (tags + "not-third"))))
    ))

example-item-data
