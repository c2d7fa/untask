#lang racket

(provide (all-defined-out))

(require
  (prefix-in item: "../core/item.rkt")
  (prefix-in prop: "../core/property.rkt")
  (prefix-in val: "../core/value.rkt")

  (prefix-in depends: "dependencies.rkt"))

;; Each task has a status, which is one of active, inactive and done. Active
;; means that the task still needs to be completed and can be worked on now;
;; inactive means that the task still needs to be done, but is not able to be
;; worked on now; and done means that the task has already been completed.

(define (calculate-status item-data item)
  (define base-status (item:get-raw-property item-data item status-property-type))
  (if (or (equal? base-status (val:make-string "inactive"))
          (equal? base-status (val:make-string "done")))
      base-status
      (if (ormap (λ (dep)
                   (not (equal? (calculate-status item-data (val:unwrap-item dep))
                                (val:make-string "done"))))
                 (set->list (val:unwrap-set (item:get-property item-data item depends:depends-property-type))))
          (val:make-string "inactive")
          (val:make-string "active"))))

(define (translate-status item-data item value)
  (item:set-raw-property item-data item status-property-type value))

(define status-property-type
  (prop:make-property-type #:key 'status
                           #:default (val:make-string "active")
                           #:calculate calculate-status
                           #:translate translate-status))
