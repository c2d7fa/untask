#lang racket

(require
 (prefix-in data: "src/data/item-data.rkt")

 (only-in "src/util.rkt" thread)

 "src/user/execute.rkt"
 "src/user/loop.rkt"

 (prefix-in status: "src/properties/status.rkt")
 (prefix-in description: "src/properties/description.rkt")
 (prefix-in tags: "src/properties/tags.rkt")
 (prefix-in urgency: "src/properties/urgency.rkt"))

(define (render-listing item-data items)
  (define (render-item item-data item)
    (format "~a. ~a [~a] <~a>"
            (data:item-id item-data item)
            (description:get-description item-data item)
            (string-join (set->list (tags:get-tags item-data item)) " ")
            (urgency:urgency item-data item)
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

(define current-item-data-box (box example-item-data))

(user-loop! current-item-data-box
            #:parse (λ (s) (read (open-input-string s)))
            #:render-listing render-listing)
