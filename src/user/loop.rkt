#lang racket

(provide user-loop!)

(require
 "execute.rkt")

(define (prompt-line prompt)
  (display prompt)
  (read-line))

(define (user-loop! item-data-box #:parse parse #:render-listing render-listing)
  (let*-values (((input) (prompt-line "> "))
                ((parsed) (parse input))
                ((new-item-data output) ((execute parsed) (unbox item-data-box))))
    (set-box! item-data-box new-item-data)
    (displayln (render-listing new-item-data output))
    (user-loop! item-data-box #:parse parse #:render-listing render-listing)))
