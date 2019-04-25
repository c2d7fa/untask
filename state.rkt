#lang racket

(require (only-in "util.rkt" set-filter))

(provide (all-defined-out))

;;; Struct definition

(struct state (next-id descriptions statuses tag-sets base-urgency) #:transparent)

;; todo: i bet there are library functions for this:
(define (state-set-next-id state- next-id)
  (state next-id (state-descriptions state-) (state-statuses state-) (state-tag-sets state-) (state-base-urgency state-)))
(define (state-set-descriptions state- descriptions)
  (state (state-next-id state-) descriptions (state-statuses state-) (state-tag-sets state-) (state-base-urgency state-)))
(define (state-set-statuses state- statuses)
  (state (state-next-id state-) (state-descriptions state-) statuses (state-tag-sets state-) (state-base-urgency state-)))
(define (state-set-tag-sets state- tag-sets)
  (state (state-next-id state-) (state-descriptions state-) (state-statuses state-) tag-sets (state-base-urgency state-)))
(define (state-set-base-urgency state- base-urgency)
  (state (state-next-id state-) (state-descriptions state-) (state-statuses state-) (state-tag-sets state-) base-urgency))

(define ((make-update get set) struct f)
  (set struct (f (get struct))))

(define state-update-next-id (make-update state-next-id state-set-next-id))
(define state-update-descriptions (make-update state-descriptions state-set-descriptions))
(define state-update-statuses (make-update state-statuses state-set-statuses))
(define state-update-tag-sets (make-update state-tag-sets state-set-tag-sets))
(define state-update-base-urgency (make-update state-base-urgency state-set-base-urgency))

;; Empty state

(define empty-state (state 0 (hash) (hash) (hash) (hash)))

;; Querying state

(define (all-ids state)
  (list->set
   (append
    (dict-keys (state-descriptions state))
    (dict-keys (state-statuses state)))))

(define (get-status id state)
  (hash-ref (state-statuses state) id 'todo))

(define (get-description id state)
  (hash-ref (state-descriptions state) id "[no description]"))

(define (get-tags id state)
  (hash-ref (state-tag-sets state) id (set)))

(define (get-base-urgency id state)
  (hash-ref (state-base-urgency state) id 0))

;; Updating state

(define (increment-id state)
  (define next-id (state-next-id state))
  (define new-state (state-update-next-id state add1))
  (values next-id new-state))

(define (set-tags id new-tags state)
  (state-update-tag-sets state (λ (tag-sets) (hash-set tag-sets id new-tags))))

(define (update-tags id f state)
  (set-tags id (f (get-tags id state)) state))

(define (add-tag id new-tag state) (update-tags id (λ (tags) (set-add tags new-tag)) state))
(define (remove-tag id old-tag state) (update-tags id (λ (tags) (set-remove tags old-tag)) state))

(define (set-status id new-status state)
  (state-update-statuses state (λ (statuses) (hash-set statuses id new-status))))

(define (set-status-done id state) (set-status id 'done state))
(define (set-status-todo id state) (set-status id 'todo state))
(define (set-status-paused id state) (set-status id 'paused state))

(define (set-description id state new-description)
  (state-update-descriptions state (λ (descriptions) (hash-set descriptions id new-description))))

(define (set-base-urgency id state base-urgency)
  (state-update-base-urgency state (λ (base-urgencies) (hash-set base-urgencies id base-urgency))))
