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

(define (done-reading-key? input)
  (and (not (equal? "" input))
       (not (equal? "\e" input))
       (not (equal? "\e[" input))
       (not (equal? "\e[3" input))))

(define (->key input)
  (cond
    ((and (string? input)
          (= 1 (string-length input)) (->key (string-ref input 0))))
    ((equal? input #\u0004) '(ctrl #\d))
    ((equal? input #\u0003) '(ctrl #\c))
    ((equal? input #\return) 'enter)
    ((equal? input #\rubout) 'backspace)
    ((equal? input "\e[A") 'up)
    ((equal? input "\e[B") 'down)
    ((equal? input "\e[C") 'right)
    ((equal? input "\e[D") 'left)
    ((equal? input "\e[H") 'home)
    ((equal? input "\e[F") 'end)
    ((equal? input "\e[3~") 'delete)
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

(define (accept line-editor k)
  (cond
    ((equal? 'enter k) (accept-eol line-editor))
    ((equal? '(ctrl #\d) k) (accept-eof line-editor))
    ((equal? '(ctrl #\c) k) (accept-eof line-editor))
    ((equal? 'backspace k) (values #f (backspace line-editor)))
    ((equal? 'home k) (values #f (home line-editor)))
    ((equal? 'end k) (values #f (end line-editor)))
    ((char? k) (accept-char line-editor k))
    (else (values #f line-editor))))
