#lang racket

(provide line-editor-empty accept output done-reading-key? ->key)

(require
  (prefix-in term: "terminal.rkt")
  (prefix-in a: "attribute.rkt")
  "squiggle.rkt")

(a:define-species line-editor (history buffer cursor))

(define line-editor-empty
  (line-editor #:history (list)
               #:buffer ""
               #:cursor 0))
(define special-keys
  '#hash((#\u0004 . (ctrl #\d))
         (#\u0003 . (ctrl #\c))
         (#\return . enter)
         (#\rubout . backspace)
         ("\e[A" . up)
         ("\e[B" . down)
         ("\e[C" . right)
         ("\e[D" . left)
         ("\e[H" . home)
         ("\e[F" . end)
         ("\e[3~" . delete)))

(define (strict-prefix? s prefix)
  (and (string-prefix? s prefix)
       (not (equal? s prefix))))

(define (may-be-start-of-special-key? s)
  (define sequences (filter string? (hash-keys special-keys)))
  (findf (lambda (seq) (strict-prefix? seq s))
         sequences))

(define (string-empty? s)
  (equal? s ""))

(define (done-reading-key? input)
  (and (not (string-empty? input))
       (not (may-be-start-of-special-key? input))))

(define (->key input)
  (define (unit-string? s)
    (and (string? s)
         (equal? (string-length s) 1)))
  (cond
    ((hash-has-key? special-keys input) (hash-ref special-keys input))
    ((unit-string? input) (->key (string-ref input 0)))
    ((char? input) input)
    (else #f)))

(define (accept-eof line-editor)
  (values "exit" line-editor))

(define (push-buffer line-editor)
  (~> line-editor
      (a:update line-editor.history (lambda (history)
                                      (cons (a:get line-editor line-editor.buffer) history)))
      (a:set line-editor.buffer "")
      (a:set line-editor.cursor 0)))

(define (accept-eol line-editor)
  (values (a:get line-editor line-editor.buffer)
          (push-buffer line-editor)))

(define (string-insert s i r)
  (string-append (substring s 0 i) r (substring s i)))

(define (insert line-editor c)
  (define cursor (a:get line-editor line-editor.cursor))
  (~> line-editor
      (a:update line-editor.buffer (λ> (string-insert cursor (string c))))
      (a:update line-editor.cursor (λ> (+ 1)))))

(define (accept-char line-editor c)
  (values #f (insert line-editor c)))

(define ((without-value f) state . args)
  (let-values (((value state) (apply f state args)))
    state))

(define (colorize-default buffer)
  `((bold) (,buffer)))

(define (output line-editor #:prompt (prompt '((bright black) ("> ")))
                            #:colorize (colorize colorize-default))
  `((clear-line)
    (((cursor-at ,(+ (term:text-length prompt)
                     (a:get line-editor line-editor.cursor)))
      (((cursor-at 0) (,prompt))
       ((cursor-at ,(term:text-length prompt)) (,(colorize (a:get line-editor line-editor.buffer)))))))))

(define (backspace line-editor)
  (~> line-editor
      (a:set line-editor.buffer "")
      (a:set line-editor.cursor 0)))

(define (home line-editor)
  (~> line-editor (a:set line-editor.cursor 0)))

(define (end line-editor)
  (define end-cursor (string-length (a:get line-editor line-editor.buffer)))
  (~> line-editor (a:set line-editor.cursor end-cursor)))

(define (move-cursor line-editor n)
  (define max-cursor (string-length (a:get line-editor line-editor.buffer)))
  (define cursor (a:get line-editor line-editor.cursor))
  (define new-cursor (max 0 (min (+ cursor n) max-cursor)))
  (~> line-editor (a:set line-editor.cursor new-cursor)))

(define (accept line-editor k)
  (cond
    ((equal? 'enter k) (accept-eol line-editor))
    ((equal? '(ctrl #\d) k) (accept-eof line-editor))
    ((equal? '(ctrl #\c) k) (accept-eof line-editor))
    ((equal? 'backspace k) (values #f (backspace line-editor)))
    ((equal? 'home k) (values #f (home line-editor)))
    ((equal? 'end k) (values #f (end line-editor)))
    ((equal? 'left k) (values #f (move-cursor line-editor -1)))
    ((equal? 'right k) (values #f (move-cursor line-editor 1)))
    ((char? k) (accept-char line-editor k))
    (else (values #f line-editor))))
