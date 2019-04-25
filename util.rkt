#lang racket

(provide (all-defined-out))

(define (set-filter pred set)
  (list->set (filter pred (set->list set))))