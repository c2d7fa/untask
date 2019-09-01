#lang racket

(provide (all-defined-out))

(require
  (prefix-in item: "../core/item.rkt")
  (prefix-in prop: "../core/property.rkt")
  (prefix-in val: "../core/value.rkt")

  (prefix-in depends: "dependencies.rkt")
  (prefix-in date: "date.rkt"))

;; Each task has a status, which is one of active, inactive and done. Active
;; means that the task still needs to be completed and can be worked on now;
;; inactive means that the task still needs to be done, but is not able to be
;; worked on now; and done means that the task has already been completed.

(define (calculate-status item-data item)
  (define base-status (item:get-raw-property item-data item status-property-type))
  (if (or (equal? base-status (val:make-string "inactive"))
          (equal? base-status (val:make-string "done")))
      base-status
      (if (or (ormap (λ (dep)
                       (not (equal? (calculate-status item-data (val:unwrap-item dep))
                                    (val:make-string "done"))))
                     (set->list (val:unwrap-set (item:get-property item-data item depends:depends-property-type))))
              (not (date:wait-active? item-data item)))
          (val:make-string "inactive")
          (val:make-string "active"))))

(define (translate-status item-data item value)
  (item:set-raw-property item-data item status-property-type value))

(define status-property-type
  (prop:property-type #:key 'status
                      #:type 'string
                      #:default (val:make-string "active")
                      #:calculate calculate-status
                      #:translate translate-status))

(define (active? item-data item)
  (equal? (val:make-string "active") (item:get-property item-data item status-property-type)))

(define (done? item-data item)
  (equal? (val:make-string "done") (item:get-property item-data item status-property-type)))
