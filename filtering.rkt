#lang racket

(require "state.rkt")
(require "item.rkt")

(provide
 (except-out (all-defined-out) set-filter))

(define (set-filter pred set)
  (list->set (filter pred (set->list set))))

;;;

(define (active? item)
  (equal? (item-status item) 'todo))

(define (uncompleted? item)
  (not (equal? (item-status item) 'done)))

(define (filter-active item-set)
  (set-filter active? item-set))

(define (filter-uncompleted item-set)
  (set-filter uncompleted? item-set))

(define (has-tag? tag item)
  (set-member? (item-tags item) tag))

(define (filter-matching-query query item-set)
  (define (matches-query? query item)
    (match query
      ('active (active? item))
      ((list 'tag tag) (has-tag? tag item))
      ((list 'not subquery) (not (matches-query? subquery item)))
      ((list 'and subqueries ...) (andmap (λ (subquery) (matches-query? subquery item)) subqueries))
      ((list 'or subqueries ...) (ormap (λ (subquery) (matches-query? subquery item)) subqueries))
      (else (error (format "unknown query type: ~s" query)))))
  (set-filter (λ (item) (matches-query? query item)) item-set))
