#lang racket

(require "item.rkt")

(provide (all-defined-out))

(define (urgency item)
  (item-base-urgency item))