#lang racket

(require
 (prefix-in data: "src/data/item-data.rkt")

 (only-in "src/util.rkt" thread)

 "src/user/execute.rkt"
 "src/user/loop.rkt"
 "src/user/listing.rkt"

 (prefix-in status: "src/properties/status.rkt")
 (prefix-in description: "src/properties/description.rkt")
 (prefix-in tags: "src/properties/tags.rkt")
 (prefix-in urgency: "src/properties/urgency.rkt")
 (only-in "src/user/parser.rkt" parse)
 )

;;; EXAMPLE

(define item-data-empty-with-properties
  (thread data:item-data-empty
    (status:register-property-base-status)
    (description:register-property-description)
    (tags:register-property-tags)
    (urgency:register-property-base-urgency)))

(define example-item-data
  (thread item-data-empty-with-properties
    (execute* (parse "add description:{this is a brand new item} tags+some-tag tags+another-tag"))
    (execute* (parse "add description:{here is another item} tags+another-tag"))
    (execute* (parse "add description:{third item} tags+some-tag tags+yet-another-tag"))
    (execute* (parse "!description/third modify tags-some-tag tags+not-third"))
    (execute* (parse "tags+not-third modify baseurgency+$2"))
    (execute* (parse "1, 2 modify baseurgency-$1"))
    ))

(define current-item-data-box (box example-item-data))

(user-loop! current-item-data-box
            #:parse parse
            #:render-listing render-listing)
