#lang racket

(provide (all-defined-out))

(require
  (prefix-in item: "./item-data.rkt"))

(define (export-item-data-to-string item-data)
  (format "~v" item-data))

(define (read-item-data-from-string item-data-string)
  (eval (read (open-input-string item-data-string))
        (let ((ns (make-base-namespace)))
          (namespace-attach-module (current-namespace) 'racket ns)
          (namespace-require 'racket ns)
          ns)))

(define (read-item-data-from-file path)
  (read-item-data-from-string (port->string (open-input-file path) #:close? #t)))
