#lang racket

(provide (all-defined-out))

(require
  (prefix-in data: "../data/item-data.rkt")
  (prefix-in prop: "../data/property-type.rkt")
  (prefix-in val: "../data/values.rkt"))

;; Each task has a status, which is one of active, inactive and done. Active
;; means that the task still needs to be completed and can be worked on now;
;; inactive means that the task still needs to be done, but is not able to be
;; worked on now; and done means that the task has already been completed.
;;
;; The status of a task is calculated based on a number of factors. One of these
;; is the so-called base status. The user sets the base status directly to one
;; of the three possible states. Even if the base status of a task is "active",
;; its true status may still be inactive if it is being blocked by another
;; dependency. A task that has been marked explicitly as "inactive" can never
;; become active until the base status is changed. Likewise, a task marked as
;; "done" can never become active.

(define base-status-property-type
  (prop:make-property-type #:key 'basestatus
                           #:default (val:make-string "active")))

;; TODO: Define 'status' property; setting this property should set the
;; base-status; reading it should calculate the actual status based on the
;; task's dependencies.
