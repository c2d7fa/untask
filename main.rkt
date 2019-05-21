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
 (prefix-in parse: "src/user/parser.rkt")
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
    (execute* '(add (and (description : (string . "this is a brand new item")) (tags + (string . "some-tag")) (tags + (string . "another-tag")))))
    (execute* '(add (and (description : (string . "here is another item")) (tags + (string . "another-tag")))))
    (execute* '(add (and (description : (string . "third item")) (tags + (string . "some-tag")) (tags + (string . "yet-another-tag")))))
    (execute* '((not (description / (string . "third"))) modify (and (tags - (string . "some-tag")) (tags + (string . "not-third")))))
    (execute* '((tags + (string . "not-third")) modify (base-urgency : (number . 2))))
    (execute* '((or (item . 1) (item . 2)) modify (base-urgency + (number . 0.5))))
    ))

(define current-item-data-box (box example-item-data))

(user-loop! current-item-data-box
            #:parse parse:parse
            #:render-listing render-listing)
