#lang racket

(provide line-editor-empty accept output)

(require
  (prefix-in term: "terminal.rkt")
  (prefix-in a: "attribute.rkt")
  "squiggle.rkt")

;; Keys {{{

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

;; }}}

(a:define-species line-editor (history history-index key-buffer buffer cursor))

(define line-editor-empty
  (line-editor #:history (list)
               #:history-index 0
               #:key-buffer (list)
               #:buffer ""
               #:cursor 0))

(define (push-buffer line-editor)
  (~> line-editor
      (a:update line-editor.history (lambda (history)
                                      (append history (list (a:get line-editor line-editor.buffer)))))
      (a:set line-editor.history-index (+ 1 (length (a:get line-editor line-editor.history))))
      (a:set line-editor.buffer "")
      (a:set line-editor.cursor 0)))

(define (pull-buffer line-editor history-index)
  (define history-length (length (a:get line-editor line-editor.history)))
  (define history-index* (max 0 (min history-length history-index)))
  (define buffer
    (if (>= history-index* history-length)
      ""
      (a:get-path (line-editor line-editor.history (a:list. history-index*)))))
  (~> line-editor
      (a:set line-editor.history-index history-index*)
      (a:set line-editor.buffer buffer)
      (a:set line-editor.cursor (string-length buffer))))

(define (move-history line-editor offset)
  (pull-buffer line-editor (+ (a:get line-editor line-editor.history-index) offset)))

(define (substring/truncated string start (end (string-length string)))
  (substring string
             (max 0 (min (string-length string) start))
             (min (string-length string) end)))

(define (string-insert s i r)
  (string-append (substring/truncated s 0 i)
                 r
                 (substring/truncated s i)))

(define (string-delete s i)
  (string-append (substring/truncated s 0 i)
                 (substring/truncated s (+ i 1))))

(define (insert line-editor c)
  (define cursor (a:get line-editor line-editor.cursor))
  (a:update line-editor line-editor.buffer (λ> (string-insert cursor (string c)))))

(define (delete line-editor)
  (define cursor (a:get line-editor line-editor.cursor))
  (a:update line-editor line-editor.buffer (λ> (string-delete cursor))))

(define (buffer-length line-editor)
  (string-length (a:get line-editor line-editor.buffer)))

(define (place-cursor line-editor i)
  (define i* (max 0 (min (buffer-length line-editor) i)))
  (~> line-editor (a:set line-editor.cursor i*)))

(define (move-cursor line-editor n)
  (place-cursor line-editor (+ (a:get line-editor line-editor.cursor) n)))

(define (colorize-default buffer)
  `((bold) (,buffer)))

(define (output line-editor #:prompt (prompt '((bright black) ("> ")))
                            #:colorize (colorize colorize-default))
  `((clear-line)
    (((cursor-at ,(+ (term:text-length prompt)
                     (a:get line-editor line-editor.cursor)))
      (((cursor-at 0) (,prompt))
       ((cursor-at ,(term:text-length prompt)) (,(colorize (a:get line-editor line-editor.buffer)))))))))

(define (home line-editor)
  (~> line-editor (a:set line-editor.cursor 0)))

(define (end line-editor)
  (define end-cursor (string-length (a:get line-editor line-editor.buffer)))
  (~> line-editor (a:set line-editor.cursor end-cursor)))

(define (accept-key line-editor k)
  (define submitted
    (cond ((equal? k '(ctrl #\d)) "exit")
          ((equal? k '(ctrl #\c)) "exit")
          ((equal? k 'enter) (a:get line-editor line-editor.buffer))
          (else #f)))
  (define (insert* le)
    (cond ((char? k) (insert le k))
          (else le)))
  (define (move* le)
    (cond ((char? k) (move-cursor le 1))
          ((equal? k 'left) (move-cursor le -1))
          ((equal? k 'right) (move-cursor le 1))
          ((equal? k 'backspace) (move-cursor le -1))
          (else le)))
  (define (position* le)
    (cond ((equal? k 'home) (home le))
          ((equal? k 'end) (end le))
          (else le)))
  (define (delete* le)
    (cond ((equal? k 'backspace) (delete le))
          ((equal? k 'delete) (delete le))
          (else le)))
  (define (push* le)
    (cond ((equal? k 'enter) (push-buffer le))
          (else le)))
  (define (history* le)
    (cond ((equal? k 'up) (move-history le -1))
          ((equal? k 'down) (move-history le 1))
          (else le)))
  (values submitted
          (~> line-editor
              (insert*)
              (move*)
              (position*)
              (delete*)
              (push*)
              (history*))))

(define (accept line-editor c)
  (define key-buffer*
    (append (a:get line-editor line-editor.key-buffer)
            (list c)))
  (define (clear-key-buffer line-editor)
    (a:set line-editor line-editor.key-buffer (list)))
  (if (done-reading-key? (apply string key-buffer*))
    (accept-key (clear-key-buffer line-editor) (->key (apply string key-buffer*)))
    (values #f (a:set line-editor line-editor.key-buffer key-buffer*))))

