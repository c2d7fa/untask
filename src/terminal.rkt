#lang racket

(provide render)

;; Takes an output description and returns a string that can be printed to
;; terminal to display the output.
(define (render output)
  "todo")

(define (butlast l)
  (reverse (cdr (reverse l))))

(define colors '(black red green yellow blue magenta cyan white))
(define (color? color) (member color colors))
(define (color-index color) (index-of colors color))

(define (csi code) (format "\e[~a" code))
(define (sgr . codes) (csi (format "~am" (string-join (map ~a codes) ";"))))

(define (translate item)
  (define (translate-code code)
    (match code
      (`(reset)
       (sgr))
      (`(bold)
       (sgr 1))
      (`(italic)
       (sgr 3))
      (`(underline)
       (sgr 4))
      (`(strikethrough)
       (sgr 9))
      (`(,color) #:when (color? color)
       (sgr (+ 30 (color-index color))))
      (`(bright ,color) #:when (color? color)
       (sgr (+ 90 (color-index color))))
      (`(,color background) #:when (color? color)
       (sgr (+ 40 (color-index color))))
      (`(bright ,color background) #:when (color? color)
       (sgr (+ 100 (color-index color))))
      ))
  (if (string? item)
      item
      (translate-code item)))

(define (translate-tree tree)
  (string-join (map translate (untree tree)) ""))

(define (untree tree #:post (post '()))
  (define (untree-tree tree)
   (define codes (butlast tree))
    (define segments (last tree))
    `(,@codes
      ,@(apply append (map (λ (segment)
                             (untree segment #:post `(,@post ,@codes)))
                           segments))
      (reset) ,@post))
  (if (string? tree) (list tree) (untree-tree tree)))

(display (translate-tree '((bright black background) (italic) (blue) ("hello, this is a " ((magenta) (underline) (strikethrough) (bold) ("test")) ".\n"))))
