#lang racket

(provide render-listing)

(require
 (prefix-in data: "../data/item-data.rkt")
 (prefix-in val: "../data/values.rkt")

 (prefix-in status: "../properties/status.rkt")
 (prefix-in description: "../properties/description.rkt")
 (prefix-in tags: "../properties/tags.rkt")
 (prefix-in urgency: "../properties/urgency.rkt")
 )

(define (render-listing item-data items)
  (define (render-item item-data item)
    (format "~a. ~a [~a] <~a>"
            (data:item-id item-data item)
            (val:unwrap-string (description:get-description item-data item))
            (string-join (map val:unwrap-string (set->list (val:unwrap-set (tags:get-tags item-data item)))) " ")
            (val:unwrap-number (urgency:urgency item-data item))
            ))
  (string-join
   (map (λ (item) (render-item item-data item)) items)
   "\n"))
