#lang racket

(provide render-listing)

(require
 (prefix-in item: "../data/item-data.rkt")
 (prefix-in val: "../data/values.rkt")

 (prefix-in status: "../properties/status.rkt")
 (prefix-in description: "../properties/description.rkt")
 (prefix-in tags: "../properties/tags.rkt")
 (prefix-in urgency: "../properties/urgency.rkt")
 (prefix-in depends: "../properties/dependencies.rkt")
 )

(define (render-listing item-data items)
  (define (render-item item-data item)
    (define description (item:get-property item-data item description:description-property-type))
    (define tags (item:get-property item-data item tags:tags-property-type))
    (define urgency (urgency:calculate-urgency item-data item))
    (define status (item:get-property item-data item status:status-property-type))
    (define depends (item:get-property item-data item depends:depends-property-type))
    (format "~a. ~a [~a] <~a> (st:~a, dp:[~a])"
            (item:item-id item-data item)
            (val:unwrap-string description)
            (string-join (map val:unwrap-string (set->list (val:unwrap-set tags))) " ")
            (val:unwrap-number urgency)
            (val:unwrap-string status)
            (string-join (map (λ (im)
                                (format "~a" (val:unwrap-item im)))
                              (set->list (val:unwrap-set depends)))
                         " ")
            ))
  (string-join
   (map (λ (item) (render-item item-data item)) items)
   "\n"))
