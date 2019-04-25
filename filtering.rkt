#lang racket

(require "state.rkt")

(provide
 (except-out (all-defined-out) set-filter))

(define (set-filter pred set)
  (list->set (filter pred (set->list set))))

;;;

(define ((item-active? #:item-status item-status) item)
  (equal? (item-status item) 'todo))

(define ((item-uncompleted? #:item-status item-status) item)
  (not (equal? (item-status item) 'done)))

(define ((filter-active #:item-status item-status) all-items)
  (set-filter (item-active? #:item-status item-status) all-items))

(define ((filter-uncompleted #:item-status item-status) all-items)
  (set-filter (item-uncompleted? #:item-status item-status) all-items))

(define ((item-has-tag? #:item-tags item-tags) item tag)
  (set-member? (item-tags item) tag))

(define ((filter-matching-query #:item-status item-status #:item-tags item-tags) all-items query)
  (define (matches-query? query item)
    (match query
      ('active ((item-active? #:item-status item-status) item))
      ((list 'tag tag) ((item-has-tag? #:item-tags item-tags) item tag))
      ((list 'not subquery) (not (matches-query? subquery item)))
      ((list 'and subqueries ...) (andmap (λ (subquery) (matches-query? subquery item)) subqueries))
      ((list 'or subqueries ...) (ormap (λ (subquery) (matches-query? subquery item)) subqueries))
      (else (error (format "unknown query type: ~s" query)))))
  (set-filter (λ (item) (matches-query? query item)) all-items))

;;; TESTING:

(define test-state
  (state
   4
   '#hash((0 . "eat food") (1 . "buy milk") (2 . "drink milk") (3 . "go for a walk"))
   '#hash((0 . paused) (1 . done))
   (hash 0 (set "health") 1 (set "shopping" "outside") 2 (set "health") 3 (set "health" "outside"))))

((filter-matching-query
  #:item-status (λ (id) (get-status id test-state))
  #:item-tags (λ (id) (get-tags id test-state)))
 (all-ids test-state) '(not (tag "health")))