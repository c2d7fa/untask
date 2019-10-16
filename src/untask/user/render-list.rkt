#lang racket

(provide render-listing
         render-listing-info
         render-agenda
         render-trees)

(require
 (prefix-in i: "../core/item.rkt")
 (prefix-in p: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt")

 (prefix-in status: "../properties/status.rkt")
 (prefix-in description: "../properties/description.rkt")
 (prefix-in tags: "../properties/tags.rkt")
 (prefix-in urgency: "../properties/urgency.rkt")
 (prefix-in depends: "../properties/dependencies.rkt")
 (prefix-in links: "../properties/links.rkt")
 (prefix-in date: "../properties/date.rkt")

 (prefix-in term: "../../terminal.rkt")
 (prefix-in dt: "../../datetime.rkt")
 )

(define (render-listing item-state items)
  (if (= 1 (length items))
      (render-listing-info item-state items)
      (render-listing* item-state items)))

(define (render-listing* item-state items)
  (define (render-item item-state item)
    (define description (p:get item-state item description:description-property))
    (define notes (p:get item-state item description:notes-property))
    (define tags (p:get item-state item tags:tags-property))
    (define urgency (p:get item-state item urgency:urgency-property))
    (define base-urgency (or (i:get item-state item urgency:urgency-property) (val:make-number 0)))
    (define status (p:get item-state item status:status-property))
    (define depends (p:get item-state item depends:depends-property))
    (define blocks (p:get item-state item depends:blocks-property))
    (define children (p:get item-state item links:children-property))
    (define parents (p:get item-state item links:parents-property))
    (define wait (p:get item-state item date:wait-property))
    (define date (p:get item-state item date:date-property))
    (term:render `(()
                   (
                    ;; ID
                    ((black)
                     (,(~r item #:min-width 3 #:pad-string " ")
                      ". "))
                    ;; Description
                    (,@(cond
                         ((status:active? item-state item) '((bold)))
                         ((status:done? item-state item) '((strikethrough) (white)))
                         (else '((white))))
                     (,(val:unwrap-string description)))
                    ;; Notes
                    ,(if (equal? "" (val:unwrap-string notes))
                         '(() ())
                         '((bold) (green) (" [..]")))
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
                    ;; Children
                    ,(if (set-empty? (val:unwrap-set children))
                         ""
                         `((red) (bold) (" C")))
                    ;; Parents
                    ,(if (set-empty? (val:unwrap-set parents))
                         ""
                         `((blue) (bold) (" P")))
                    ;; Wait
                    ,(if (not (date:wait-active? item-state item))  ; Display only when task is waiting
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
   (map (λ (item) (render-item item-state item)) items)
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
        `(,@colors (,@(if (dt:this-year? d) '()
                         `(,(~a (dt:datetime-year d)) "-"))
                    ,(dt:month-short-string d) "-"
                    ,(~ (dt:datetime-day d))
                    ((reset) (black) ("T"))
                    ,(~ (dt:datetime-hour d)) ":"
                    ,(~ (dt:datetime-minute d))))
        `(,@colors (,@(if (dt:this-year? d) '()
                         `(,(~a (dt:datetime-year d)) "-"))
                    ,(dt:month-short-string d) "-"
                    ,(~ (dt:datetime-day d)))))))

(define (render-listing-info item-state items)
  (define (render-item item)
    (define description (p:get item-state item description:description-property))
    (define notes (p:get item-state item description:notes-property))
    (define tags (p:get item-state item tags:tags-property))
    (define urgency (p:get item-state item urgency:urgency-property))
    (define base-urgency (or (i:get item-state item urgency:urgency-property) (val:make-number 0)))
    (define status (p:get item-state item status:status-property))
    (define depends (p:get item-state item depends:depends-property))
    (define blocks (p:get item-state item depends:blocks-property))
    (define children (p:get item-state item links:children-property))
    (define parents (p:get item-state item links:parents-property))
    (define wait (p:get item-state item date:wait-property))
    (define date (p:get item-state item date:date-property))
    (term:render `(()
                   (
                    ;; Description
                    ((bold)
                     (,(val:unwrap-string description)))
                    "\n\n"
                    ;; Notes
                    (()
                     (,(if (not (equal? "" (val:unwrap-string notes)))
                           (string-append (val:unwrap-string notes) "\n\n")
                           "")))
                    ;; Id
                    (()
                     (((black) ("ID:      "))
                      ,(~a item)))
                    "\n"
                    ;; Status
                    (()
                     (((black) ("Status:  "))
                      ,(cond
                         ((status:active? item-state item) '((bold) ("active")))
                         ((status:done? item-state item) '((strikethrough) (white) ("done")))
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
                            ,(string-indent (render-listing* item-state (map val:unwrap-item (set->list (val:unwrap-set depends))))
                                            4))))
                    ;; Blocked
                    ,(if (set-empty? (val:unwrap-set blocks))
                         ""
                         `(()
                           ("\n\n"
                            ((black) ("Blocks:\n"))
                            ,(string-indent (render-listing* item-state (map val:unwrap-item (set->list (val:unwrap-set blocks))))
                                            4))))
                    ;; Children
                    ,(if (set-empty? (val:unwrap-set children))
                         ""
                         `(()
                           ("\n\n"
                            ((black) ("Children:\n"))
                            ,(string-indent (render-listing* item-state (map val:unwrap-item (set->list (val:unwrap-set children))))
                                            4))))
                    ;; Parents
                    ,(if (set-empty? (val:unwrap-set parents))
                         ""
                         `(()
                           ("\n\n"
                            ((black) ("Parents:\n"))
                            ,(string-indent (render-listing* item-state (map val:unwrap-item (set->list (val:unwrap-set parents))))
                                            4))))
                    ))))
  (string-join
   (map (λ (item) (render-item item)) items)
   "\n\n\n"))

(define (render-agenda item-state blocks)
  ;; `blocks' is a list of blocks of the form (date . items), where `date' is a
  ;; datetime as defined in the datetime module and `items' is a list of items
  ;; to display for that date.
  (define (style-date d)
    (define (~ x) (~r x #:min-width 2 #:pad-string "0"))
    (let ((colors (cond
                    ((dt:future? d) `((bold)))
                    ((dt:today? d) `((blue) (bold)))
                    (else `((red) (bold))))))
      `(() ((,@colors (,(dt:weekday-short-string d) " "
                       ,(~a (dt:datetime-year d)) "-"
                       ,(dt:month-short-string d) "-"
                       ,(~ (dt:datetime-day d))))
            " "
            ((black) ("(" ((bold) (,(~r (dt:days-from-today d) #:sign '++) "d")) ")"))))))
  (define (render-block block)
    (term:render `(() (,(style-date (car block))
                       "\n"
                       ,(string-indent (render-listing* item-state (cdr block)) 4)
                       ))))
  (string-join
   (map (λ (block) (render-block block)) blocks)
   "\n\n"))

(define (render-trees item-state trees)
  (define (render-tree tree)
    (if (null? tree)
        (term:render '((black) ("   ...")))
        (string-join
         (filter (λ (s) (not (equal? "" s)))
                 (list (render-listing* item-state (list (car tree)))
                       (string-indent (render-trees item-state (cdr tree)) 2)))
         "\n")))
  (string-join
   (map render-tree trees)
   "\n"))
