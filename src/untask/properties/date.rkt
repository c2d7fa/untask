#lang racket

(provide wait-property
         date-property
         wait-active?
         copy-recur)

(require
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in val:  untask/src/untask/core/value)
 (prefix-in dt: untask/src/datetime))

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
;; parameters.
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
  #f)
