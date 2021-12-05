#lang racket

(provide render with-raw! display! text-length)

(require racket/system (for-syntax racket/syntax))

;; Takes an output description and returns a string that can be printed to
;; terminal to display the output.
(define (render output)
  (translate-tree output))

(define (display! output)
  (display (render output))
  (flush-output))

(define-syntax-rule (with-raw! body ...)
  (begin (system "stty raw -echo")
         (let ((result (begin body ...)))
           (system "stty -raw echo")
           result)))

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
      (`(cursor-at ,x)
       (format "\e[~aG" (+ x 1)))
      (`(clear-line)
       (format "\e[2K"))
      ('() "")))
  (if (string? item)
      (fix-newlines-for-raw-output item)
      (translate-code item)))

(define (text-length tree)
  (cond
    ((string? tree) (string-length tree))
    ((list? tree) (foldl + 0 (map text-length tree)))
    (else 0)))

(define (translate-tree tree)
  (string-join (map translate (untree tree)) ""))

(define (untree tree #:post (post '()))
  (define (untree-tree tree)
    (define codes (butlast tree))
    (define post-codes (filter (lambda (code)
                                 (and (not (equal? code '(clear-line)))
                                      (not (and (list? code)
                                                (not (equal? '() code))
                                                (equal? (car code) 'cursor-at)))))
                               codes))
    (define segments (last tree))
    `(,@codes
      ,@(apply append (map (λ (segment)
                             (untree segment #:post `(,@post ,@post-codes)))
                           segments))
      (reset) ,@post))
  (if (string? tree) (list tree) (untree-tree tree)))
