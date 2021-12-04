#lang racket

(provide line-editor-empty accept output)

(require
  (prefix-in term: "terminal.rkt")
  (prefix-in a: "attribute.rkt")
  "squiggle.rkt")

(a:define-species line-editor (history buffer cursor))

;; TODO: Read escape sequences. For example Up is (#\u001B #\[ #\A).

(define line-editor-empty
  (line-editor #:history (list)
               #:buffer ""
               #:cursor 0))

(define (eol? c)
  (eq? c #\return))
(define (eof? c)
  (or (eq? c #\u0004)
      (eq? c #\u0003)))

(define (accept line-editor c)
  (cond
    ((eol? c) (accept-eol line-editor))
    ((eof? c) (accept-eof line-editor))
    (else (accept-char line-editor c))))

(define (accept-eof line-editor)
  (values 'eof line-editor))

(define (push-buffer line-editor)
  (~> line-editor
      (a:update line-editor.history (lambda (history)
                                      (cons (a:get line-editor line-editor.buffer) history)))
      (a:set line-editor.buffer "")
      (a:set line-editor.cursor 0)))

(define (accept-eol line-editor)
  (values (a:get line-editor line-editor.buffer)
          (push-buffer line-editor)))

(define (insert line-editor c)
  (~> line-editor
      (a:update line-editor.buffer (λ> (string-append (string c))))
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
  `((cursor-at ,(+ (term:text-length prompt)
                   (a:get line-editor line-editor.cursor)))
    (((cursor-at 0) (,prompt))
     ((cursor-at ,(term:text-length prompt)) (,(colorize (a:get line-editor line-editor.buffer)))))))

