#lang racket

(require (prefix-in status: "status.rkt")
         (prefix-in tags: "tags.rkt")
         (only-in "util.rkt" set-filter filter-item-set))

(provide (all-defined-out))

;;;


(define (filter-active item-data item-set)
  (filter-item-set item-data item-set status:active?))

(define (filter-uncompleted item-data item-set)
  (filter-item-set item-data item-set status:unfinished?))

(define (filter-matching-query item-data item-set query)
  (define (matches-query? item query)
    (match query
      ('active (status:active? item-data item))
      (`(tag ,tag) (tags:has-tag? item-data item tag))
      (`(not ,subquery) (not (matches-query? subquery item)))
      (`(and ,subqueries ...) (andmap (λ (subquery) (matches-query? item subquery)) subqueries))
      (`(or ,subqueries ...) (ormap (λ (subquery) (matches-query? item subquery)) subqueries))
      (else (error (format "unknown query type: ~s" query)))))
  (set-filter
   (λ (item)
     (matches-query? item query))
   item-set))
