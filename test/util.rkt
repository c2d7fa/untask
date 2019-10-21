#lang racket

(provide (all-defined-out))

(define (capture-output proc)
  (regexp-replace* #rx"\e.*?m"
                   (with-output-to-string proc)
                   ""))

;; Return #t if the lists represent the same set, #f otherwise.
(define (same-set? l m)
  (equal? (list->set l) (list->set m)))
