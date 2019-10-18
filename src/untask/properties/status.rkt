#lang racket

(provide status-property
         active?
         done?)

(require
 (prefix-in i: "../core/item.rkt")
 (prefix-in p: "../core/property.rkt")
 (prefix-in val: "../core/value.rkt")
 "../../squiggle.rkt"

 (prefix-in depends: "dependencies.rkt")
 (prefix-in date: "date.rkt"))

;; Each task has a status, which is one of active, inactive and done. Active
;; means that the task still needs to be completed and can be worked on now;
;; inactive means that the task still needs to be done, but is not able to be
;; worked on now; and done means that the task has already been completed.

(define (calculate-status item-state item)
  (define base-status (i:get item-state item 'status))
  (if (or (equal? base-status (val:make-string "inactive"))
          (equal? base-status (val:make-string "done")))
      base-status
      (if (or (ormap (λ (dep)
                       (not (equal? (calculate-status item-state (val:unwrap-item dep))
                                    (val:make-string "done"))))
                     (set->list (val:unwrap-set (p:get item-state item depends:depends-property))))
              (not (date:wait-active? item-state item)))
          (val:make-string "inactive")
          (val:make-string "active"))))

(define (translate-status item-state item value)
  (i:set item-state item 'status value))

(define status-property
  (~> (p:property #:name 'status
                  #:type 'string
                  #:calculate calculate-status
                  #:translate translate-status)
      (p:default (val:make-string "active"))))

(define (active? item-state item)
  (equal? (val:make-string "active") (p:get item-state item status-property)))

(define (done? item-state item)
  (equal? (val:make-string "done") (p:get item-state item status-property)))
