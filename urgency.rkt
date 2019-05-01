#lang racket

;; EXAMPLE OF NEW PROPERTY SYSTEM

(define-item-property base-urgency
  #:key 'urgency
  #:type 'number
  #:default 0.0)

(define (urgency item)
  (define age-multiplier ...)
  (* (item-base-urgency item) age-multiplier))

;; Comment: I dislike how urgency acts on some "item" argument and returns an
;; arbitrary value, while this function acts on state and a bare item-id and
;; necessarily returns new-state.
;;
;; Seems like it would be more elegant to explicitly distinguish between two
;; kinds of functions: one that takes a state and any number of arguments (like
;; a reference to an item), and one that takes a state and any number of
;; arguments (again, including item referneces) and returns both an updated
;; state and any additional values. These function types could be combined in a
;; consistent way. This is basically the Reader and State monads.
;;
;; I tried doing an implementation of this concept, but it just seems too
;; annoying to have to deal with without some sort of language-level support for
;; monads. An okay compromise might be to simply unify the interfaces of these
;; functions so that they always take state as their first argument, and always
;; return a new state, even if it hasn't been modified.
(define (boost-urgency-to-top state item-id)
  (define max-urgency (max (get-all-items) #:key urgency))
  (define new-state (set-property state item-id (add1 max-urgency)))
  new-state)

;; END EXAMPLE






(require "item.rkt")

(provide (all-defined-out))

(define (urgency item)
  (item-base-urgency item))

(define (sort-items-by-urgency-descending items)
  (sort (set->list items) >= #:key urgency))
