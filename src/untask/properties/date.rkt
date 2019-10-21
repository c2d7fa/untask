#lang racket

(provide wait-property
         date-property
         wait-active?
         copy-recur)

(require
 (prefix-in i: untask/src/untask/core/item)
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in val:  untask/src/untask/core/value)
 (prefix-in dt: untask/src/datetime)
 untask/src/squiggle)

(define wait-property
  (p:property #:name 'wait
              #:type '(opt date)))

(define date-property
  (p:property #:name 'date
              #:type '(opt date)))

(define (wait-active? item-state item)
  (let ((wait (p:get item-state item wait-property)))
    (or (not wait)
        (not (dt:future? (val:unwrap-date wait))))))

;; Make copies of a task corresponding to recurrences of that task. The copies
;; will have their date and wait properties set according to the given
;; parameters. Returns as values the updated item state and the list of newly
;; created items.
;;
;; The first task will be created on the date given by 'start' and no tasks will
;; be created after 'end'. Each task will be separated by 'skip' (which is an
;; integer) days.
;;
;; The 'wait' property will be determined by the 'date' property such that the
;; time between the two is constant. If the item being copies does not have any
;; 'wait', then the resulting items also won't have any 'wait'.
;;
;; If the source item does not exist, do nothing.
(define (copy-recur item-state item #:start start #:end end #:skip skip)
  (define wait-offset
    (let ((w (p:get item-state item wait-property))
          (d (p:get item-state item date-property)))
      (if (and w d)
          (dt:days-between (val:unwrap-date w) (val:unwrap-date d))
          #f)))
  (let ((item-state/new-items
         (foldl (λ (d item-state/new-items)
                  (let ((item-state (car item-state/new-items))
                        (new-items  (cdr item-state/new-items)))
                    (let-values (((item-state* item*) (i:clone item-state item)))
                      (~>! item-state*
                           (p:set item* date-property (val:make-date d)))
                      (when wait-offset
                        (~>! item-state*
                             (p:set item* wait-property (val:make-date (dt:add-days d wait-offset)))))
                      (cons item-state* (append new-items (list item*))))))
                (cons item-state (list))
                (dt:date-range start end skip))))
    (values (car item-state/new-items)
            (cdr item-state/new-items))))
