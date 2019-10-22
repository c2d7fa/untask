#lang racket

(provide (all-defined-out))

(require pkg/lib
         untask/src/untask/core/serialize)

(define (capture-output proc)
  (regexp-replace* #rx"\e.*?m"
                   (with-output-to-string proc)
                   ""))

;; Return #t if the lists represent the same set, #f otherwise.
(define (same-set? l m)
  (equal? (list->set l) (list->set m)))

(define (load-example name)
  (load-state (path->string (build-path (pkg-directory "untask") "test/examples/" name))))
