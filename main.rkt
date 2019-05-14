#lang racket

(require
 (prefix-in data: "item-data.rkt")

 (only-in "util.rkt" thread)

 "execute.rkt"

 (prefix-in status: "status.rkt")
 (prefix-in description: "description.rkt")
 (prefix-in tags: "tags.rkt")
 (prefix-in urgency: "urgency.rkt"))

(define (render-listing item-data items)
  (define (render-item item-data item)
    (format "* <~a> ~a [~a]"
            (urgency:urgency item-data item)
            (description:get-description item-data item)
            (string-join (set->list (tags:get-tags item-data item)) " ")
            )
    )
  (string-join
   (map (λ (item) (render-item item-data item)) items)
   "\n"))

;;; EXAMPLE

(define item-data-empty-with-properties
  (thread data:item-data-empty
    (status:register-property-base-status)
    (description:register-property-description)
    (tags:register-property-tags)
    (urgency:register-property-base-urgency)))

(define example-item-data
  (thread item-data-empty-with-properties
    (execute* '(add (and (description : "this is a brand new item") (tags + "some-tag") (tags + "another-tag"))))
    (execute* '(add (and (description : "here is another item") (tags + "another-tag"))))
    (execute* '(add (and (description : "third item") (tags + "some-tag") (tags + "yet-another-tag"))))
    (execute* '((not (description / "third")) modify (and (tags - "some-tag") (tags + "not-third"))))
    (execute* '((tags + "not-third") modify (base-urgency : 2)))
    ))

(displayln (render-listing example-item-data (set->list (data:all-items example-item-data))))
