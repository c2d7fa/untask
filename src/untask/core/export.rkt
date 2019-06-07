#lang racket

(provide (all-defined-out))

(require
  (prefix-in state: "./item.rkt"))

(define (export-state-to-string state)
  (format "~v" state))

(define (read-state-from-string item-data-string)
  (eval (read (open-input-string item-data-string))
        (let ((ns (make-base-namespace)))
          (namespace-attach-module (current-namespace) 'racket ns)
          (namespace-require 'racket ns)
          ns)))

(define (read-state-from-file path)
  (read-state-from-string (port->string (open-input-file path) #:close? #t)))
