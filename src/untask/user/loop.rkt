#lang racket

(provide user-loop!)

(require
 "../../untask/user/render-list.rkt"
 "../../untask/user/parser.rkt"
 "../../untask/user/execute.rkt"

 "../../untask/core/state.rkt"
 (prefix-in c: "../../untask/core/context.rkt")
 (prefix-in a: "../../attribute.rkt")
 (prefix-in term: "../../terminal.rkt")

 "../../squiggle.rkt")

(define (read-line-raw!)
  (define (eol? c) (eq? c #\return))
  (define (eof? c) (eq? c #\u0004))
  (define buffer "")
  (define (continue-reading!)
    (let ((c (read-char)))
      (when (not (or (eol? c)
                     (eof? c)))
        (set! buffer (string-append buffer (string c))))
      (cond
        ((eol? c) (term:display! '(() ("\n")))
                  buffer)
        ((eof? c) #f)
        (else (term:display! `(() (,(string c))))
              (continue-reading!)))))
  (continue-reading!))

;; Print prompt and return input. Returns #f if user interrupts program while
;; waiting for input.
(define (prompt-line prompt)
  (term:with-raw!
    (term:display! prompt)
    (read-line-raw!)))

(define (try-parse input)
  (with-handlers ((exn? (λ (e) '(parse-error))))
    (if (not input) '(exit) (parse input))))

(define (format-prompt-line #:open-file open-file #:current-contexts current-contexts)
  (let ((contexts (string-join
                   (map (λ (c)
                          (term:render
                           `(() (((bright black) ("@"))
                                 ((cyan) (,c))))))
                        current-contexts)
                   " ")))
    (term:render `(()
                   (,contexts
                    ((bright black) (,(if open-file (if (set-empty? current-contexts) open-file (format " ~A" open-file)) "") "> ")))))))

(define (user-loop! state-box)
  (let* ((input (prompt-line (format-prompt-line #:open-file (a:get-path ((unbox state-box) state.open-file))
                                                 #:current-contexts (~> (a:get-path ((unbox state-box) state.context-state))
                                                                        (c:activated)))))
         (command (with-handlers ((exn:fail:read? (λ (e) (displayln (term:render '((bold) (red) ("Error: Unable to parse command.")))) #f)))
                    (if input (parse input) '(exit))))
         (result (if command
                     (execute! command state-box)
                     'proceed)))
    (when (equal? 'proceed result)
      (user-loop! state-box))))
