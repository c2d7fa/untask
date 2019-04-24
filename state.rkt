#lang racket

(provide
 (except-out (all-defined-out) set-filter))

(define (set-filter pred set)
  (list->set (filter pred (set->list set))))

;;; Struct definition

(struct state (next-id descriptions statuses tag-sets) #:transparent)

;; todo: i bet there are library functions for this:
(define (state-set-next-id state- next-id)
  (state next-id (state-descriptions state-) (state-statuses state-) (state-tag-sets state-)))
(define (state-set-descriptions state- descriptions)
  (state (state-next-id state-) descriptions (state-statuses state-) (state-tag-sets state-)))
(define (state-set-statuses state- statuses)
  (state (state-next-id state-) (state-descriptions state-) statuses (state-tag-sets state-)))
(define (state-set-tag-sets state- tag-sets)
  (state (state-next-id state-) (state-descriptions state-) (state-statuses state-) tag-sets))
(define ((make-update get set) struct f)
  (set struct (f (get struct))))
(define state-update-next-id (make-update state-next-id state-set-next-id))
(define state-update-descriptions (make-update state-descriptions state-set-descriptions))
(define state-update-statuses (make-update state-statuses state-set-statuses))
(define state-update-tag-sets (make-update state-tag-sets state-set-tag-sets))

;; Empty state

(define empty-state (state 0 (hash) (hash) (hash)))

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

(define (is-active id state)
  (equal? (get-status id state) 'todo))

(define (all-active-ids state)
  (set-filter (λ (id) (is-active id state)) (all-ids state)))
(define (all-uncompleted-ids state)
  (set-filter (λ (id) (not (equal? (get-status id state) 'done))) (all-ids state)))

(define (has-tag? id tag state)
  (set-member? (get-tags id state) tag))

(define (search-ids-matching-query query state)
  (define (matches-query? query id state)
    (cond
      ((equal? query 'active) (is-active id state))
      ((equal? (car query) 'has-tag) (has-tag? id (cadr query) state))
      ((equal? (car query) 'not) (not (matches-query? (cadr query) id state)))
      ((equal? (car query) 'and) (andmap (λ (q) (matches-query? q id state)) (cdr query)))
      ((equal? (car query) 'or) (ormap (λ (q) (matches-query? q id state)) (cdr query)))
      (else (error (format "unknwon query type: ~s" query)))))
  (set-filter (λ (id) (matches-query? query id state)) (all-ids state)))

;; Updating state

(define (set-tags id new-tags state)
  (state-update-tag-sets state (λ (tag-sets) (hash-set tag-sets id new-tags))))
(define (update-tags id f state)
  (set-tags id (f (get-tags id state)) state))
(define (add-tag id new-tag state)
  (update-tags id (λ (tags) (set-add tags new-tag)) state))
(define (remove-tag id old-tag state)
  (update-tags id (λ (tags) (set-remove tags old-tag)) state))

(define (set-status id new-status state)
  (state-update-statuses state (λ (statuses) (hash-set statuses id new-status))))
(define (set-status-done id state) (set-status id 'done state))
(define (set-status-todo id state) (set-status id 'todo state))
(define (set-status-paused id state) (set-status id 'paused state))

(define (set-description id state new-description)
  (state-update-descriptions state (λ (descriptions) (hash-set descriptions id new-description))))

(define (increment-id state)
  (define next-id (state-next-id state))
  (define new-state (state-update-next-id state add1))
  (values next-id new-state))