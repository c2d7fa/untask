#lang racket

(provide render with-raw! display!)

(require racket/system)

;; Takes an output description and returns a string that can be printed to
;; terminal to display the output.
(define (render output)
  (translate-tree output))

(define (display! output)
  (display (render output))
  (flush-output))

(define (with-raw! f)
  (system "stty raw -echo")
  (f)
  (system "stty -raw echo"))

(define (butlast l)
  (reverse (cdr (reverse l))))

(define colors '(black red green yellow blue magenta cyan white))
(define (color? color) (member color colors))
(define (color-index color) (index-of colors color))

(define (csi code) (format "\e[~a" code))
(define (sgr . codes) (csi (format "~am" (string-join (map ~a codes) ";"))))

(define (fix-newlines-for-raw-output s)
  (string-replace s "\n" "\n\r"))

(define (translate item)
  (define (translate-code code)
    (match code
      (`(reset)
       (sgr))
      (`(bold)
       (sgr 1))
      (`(faint)
       (sgr 2))
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
      ('() "")))
  (if (string? item)
      (fix-newlines-for-raw-output item)
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
