#lang racket

(provide render-listing
         render-listing-info)

(require
 (prefix-in item: "../core/item.rkt")
 (prefix-in val: "../core/value.rkt")

 (prefix-in status: "../properties/status.rkt")
 (prefix-in description: "../properties/description.rkt")
 (prefix-in tags: "../properties/tags.rkt")
 (prefix-in urgency: "../properties/urgency.rkt")
 (prefix-in depends: "../properties/dependencies.rkt")
 (prefix-in date: "../properties/date.rkt")

 (prefix-in term: "../../terminal.rkt")
 (prefix-in dt: "../../datetime.rkt")
 )

(define (render-listing item-data items)
  (if (= 1 (length items))
      (render-listing-info item-data items)
      (render-listing* item-data items)))

(define (render-listing* item-data items)
  (define (render-item item-data item)
    (define description (item:get-property item-data item description:description-property-type))
    (define tags (item:get-property item-data item tags:tags-property-type))
    (define urgency (item:get-property item-data item urgency:urgency-property-type))
    (define base-urgency (item:get-raw-property item-data item urgency:urgency-property-type))
    (define status (item:get-property item-data item status:status-property-type))
    (define depends (item:get-property item-data item depends:depends-property-type))
    (define blocks (item:get-property item-data item depends:blocks-property-type))
    (define wait (item:get-property item-data item date:wait-property-type))
    (define date (item:get-property item-data item date:date-property-type))
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
                           (((black) (" B:"))
                            ((bold) (,(~a (set-count (val:unwrap-set blocks))))))))
                    ;; Depends
                    ,(if (set-empty? (val:unwrap-set depends))
                         ""
                         `((blue)
                           (((black) (" D:"))
                            ((bold) (,(~a (set-count (val:unwrap-set depends))))))))
                    ;; Wait
                    ,(if (not (date:wait-active? item-data item))  ; Display only when task is waiting
                         `(() (((black) (" W:"))
                               ,(style-date (val:unwrap-date wait))))
                         (if wait                                  ; If task has "wait" but is not waiting, simply display "W"
                             `((black) (" W"))
                             ""))
                    ;; Date
                    ,(if (not date)
                         ""
                         `(() (((black) (" D:"))
                               ,(style-date (val:unwrap-date date)))))
                    ))))
  (string-join
   (map (λ (item) (render-item item-data item)) items)
   "\n"))

(define (string-indent s n)
  (string-join (map (λ (s) (string-append (make-string n #\space) s))
                    (string-split s "\n"))
               "\n"))

(define (style-date d)
  (define (~ x) (~r x #:min-width 2 #:pad-string "0"))
  (let ((colors (cond
                  ((dt:future? d) `((green)))
                  ((dt:today? d) `((yellow) (bold)))
                  (else `((red) (bold))))))
    (if (dt:has-time? d)
        `(,@colors (,(~a (dt:datetime-year d)) "-"
                    ,(~ (dt:datetime-month d)) "-"
                    ,(~ (dt:datetime-day d))
                    ((reset) (black) ("T"))
                    ,(~ (dt:datetime-hour d)) ":"
                    ,(~ (dt:datetime-minute d))))
        `(,@colors (,(~a (dt:datetime-year d)) "-"
                    ,(~ (dt:datetime-month d)) "-"
                    ,(~ (dt:datetime-day d)))))))

(define (render-listing-info item-data items)
  (define (render-item item)
    (define description (item:get-property item-data item description:description-property-type))
    (define tags (item:get-property item-data item tags:tags-property-type))
    (define urgency (item:get-property item-data item urgency:urgency-property-type))
    (define base-urgency (item:get-raw-property item-data item urgency:urgency-property-type))
    (define status (item:get-property item-data item status:status-property-type))
    (define depends (item:get-property item-data item depends:depends-property-type))
    (define blocks (item:get-property item-data item depends:blocks-property-type))
    (define wait (item:get-property item-data item date:wait-property-type))
    (define date (item:get-property item-data item date:date-property-type))
    (term:render `(()
                   (
                    ;; Description
                    ((bold)
                     (,(val:unwrap-string description)))
                    "\n\n"
                    ;; Id
                    (()
                     (((black) ("ID:      "))
                      ,(~a (item:item-id item-data item))))
                    "\n"
                    ;; Status
                    (()
                     (((black) ("Status:  "))
                      ,(cond
                         ((status:active? item-data item) '((bold) ("active")))
                         ((status:done? item-data item) '((strikethrough) (white) ("done")))
                         (else `((white) (,(val:unwrap-string status)))))))
                    "\n"
                    ;; Wait
                    ,(if wait
                         `(()
                           (((black) ("Wait:    "))
                            ,(style-date (val:unwrap-date wait))
                            "\n"))
                         '(() ()))
                    ;; Date
                    ,(if date
                         `(()
                           (((black) ("Date:    "))
                            ,(style-date (val:unwrap-date date))
                            "\n"))
                         '(() ()))
                    ;; Urgency
                    (()
                     (((black) ("Urgency: "))
                      ((bold) (yellow) (,(~a (val:unwrap-number urgency))))
                      ,(if (equal? urgency base-urgency)
                           `(() ())
                           `((black) (" (Base " ((yellow) (,(~a (val:unwrap-number base-urgency)))) ")")))))
                    "\n"
                    ;; Tags
                    (()
                     (((black) ("Tags:    "))
                      ,(string-join
                        (map (λ (t)
                               (term:render
                                `(()
                                  (((black) ("#"))
                                   ((blue) (,(val:unwrap-string t)))))))
                             (set->list (val:unwrap-set tags)))
                        " ")))
                    ;; Dependencies
                    ,(if (set-empty? (val:unwrap-set depends))
                         ""
                         `(()
                           ("\n\n"
                            ((black) ("Depends on:\n"))
                            ,(string-indent (render-listing* item-data (map val:unwrap-item (set->list (val:unwrap-set depends))))
                                            4))))
                    ;; Blocked
                    ,(if (set-empty? (val:unwrap-set blocks))
                         ""
                         `(()
                           ("\n\n"
                            ((black) ("Blocks:\n"))
                            ,(string-indent (render-listing* item-data (map val:unwrap-item (set->list (val:unwrap-set blocks))))
                                            4))))
                    ))))
  (string-join
   (map (λ (item) (render-item item)) items)
   "\n\n\n"))
