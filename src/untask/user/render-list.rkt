#lang racket

(provide render-listing)

(require
 (prefix-in item: "../core/item.rkt")
 (prefix-in val: "../core/value.rkt")

 (prefix-in status: "../properties/status.rkt")
 (prefix-in description: "../properties/description.rkt")
 (prefix-in tags: "../properties/tags.rkt")
 (prefix-in urgency: "../properties/urgency.rkt")
 (prefix-in depends: "../properties/dependencies.rkt")

 (prefix-in term: "../../terminal.rkt")
 )

(define (render-listing item-data items)
  (define (render-item item-data item)
    (define description (item:get-property item-data item description:description-property-type))
    (define tags (item:get-property item-data item tags:tags-property-type))
    (define urgency (item:get-property item-data item urgency:urgency-property-type))
    (define base-urgency (item:get-raw-property item-data item urgency:urgency-property-type))
    (define status (item:get-property item-data item status:status-property-type))
    (define depends (item:get-property item-data item depends:depends-property-type))
    (define blocks (item:get-property item-data item depends:blocks-property-type))
    (term:render `(()
                   (
                    ;; ID
                    ((black)
                     (,(~a (item:item-id item-data item))
                      ". "))
                    ;; Description
                    (,@(cond
                         ((status:active? item-data item) '((bold)))
                         ((status:done? item-data item) '((strikethrough) (white)))
                         (else '((white))))
                     (,(val:unwrap-string description)))
                    ;; Tags
                    " "
                    ,(string-join
                      (map (λ (t)
                             (term:render
                              `(()
                                (((black) ("#"))
                                 ((blue) (,(val:unwrap-string t)))))))
                           (set->list (val:unwrap-set tags)))
                      " ")
                    ;; Urgency
                    " "
                    ((bold) (yellow)
                      (,(~a (val:unwrap-number urgency))))
                    ;; Blocks
                    ,(if (set-empty? (val:unwrap-set blocks))
                         ""
                         `((red)
                           ((() (" B:"))
                            ((bold) (,(~a (set-count (val:unwrap-set blocks))))))))
                    ;; Depends
                    ,(if (set-empty? (val:unwrap-set depends))
                         ""
                         `((blue)
                           ((() (" D:"))
                            ((bold) (,(~a (set-count (val:unwrap-set depends))))))))
                    ))))
  (string-join
   (map (λ (item) (render-item item-data item)) items)
   "\n"))
