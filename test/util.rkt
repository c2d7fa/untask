#lang racket

(provide (all-defined-out))

(define (capture-output proc)
  (regexp-replace* #rx"\e.*?m"
                   (with-output-to-string proc)
                   ""))
