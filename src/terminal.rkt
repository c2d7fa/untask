#lang racket

(provide render)

;; Takes an output description and returns a string that can be printed to
;; terminal to display the output.
(define (render output)
  "todo")

(define (butlast l)
  (reverse (cdr (reverse l))))

(define (untree tree #:post (post '()))
  (define (untree-tree tree)
    (define codes (butlast tree))
    (define segments (last tree))
    `(,@codes
      ,@(apply append (map (λ (segment)
                             (untree segment #:post `(,@post ,@codes)))
                           segments))
      ,@post))
  (if (string? tree) (list tree) (untree-tree tree)))

(untree '((bright green)
          (yellow background)
          ("here is some text. "
           ((magenta)
            ("here is some more. "))
           "finally there is this. ")))
