#lang racket

(provide (all-defined-out))

(require
  (prefix-in item: "./item-data.rkt"))

(define (export-item-data-to-string item-data)
  ;; TODO: Use a more robust serialization approach.
  (format "~v" item-data))

(define (read-item-data-from-string item-data-string)
  ;; TODO: Probably don't (eval (read)). Also I have no idea
  ;; what I'm doing with this namespace stuff.
  (eval (read (open-input-string item-data-string))
        (let ((ns (make-base-namespace)))
          (namespace-attach-module (current-namespace) 'racket ns)
          (namespace-require 'racket ns)
          ns)))