#lang racket

(provide user-loop!)

(require
 "render-list.rkt"
 "parser.rkt"
 "execute.rkt"

 (prefix-in state: "../core/state.rkt")
 (prefix-in ctx: "../core/context.rkt")
 (prefix-in a: "../../attribute.rkt")
 (prefix-in term: "../../terminal.rkt")

 (only-in "../../misc.rkt" try-read-line*)
 "../../squiggle.rkt")

;; Print prompt and return input. Returns #f if user interrupts program while
;; waiting for input.
(define (prompt-line prompt)
  (display prompt)
  (try-read-line*))

(define (try-parse input)
  (with-handlers ((exn? (λ (e) '(parse-error))))
    (if (not input) '(exit) (parse input))))

(define (format-prompt-line #:open-file open-file #:current-contexts current-contexts)
  (let ((contexts (string-join
                   (map (λ (c)
                          (term:render
                           `(() (((black) ("@"))
                                 ((cyan) (,c))))))
                        current-contexts)
                   " ")))
    (term:render `(()
                   (,contexts
                    ((black) (,(if open-file (if (set-empty? current-contexts) open-file (format " ~A" open-file)) "") "> ")))))))

(define (user-loop! state-box)
  (let* ((input (prompt-line (format-prompt-line #:open-file (a:get-path ((unbox state-box) state:state.open-file))
                                                 #:current-contexts (~> (a:get-path ((unbox state-box) state:state.context-state))
                                                                        (ctx:activated-names)))))
         (command (with-handlers ((exn:fail:read? (λ (e) (displayln (term:render '((bold) (red) ("Error: Unable to parse command.")))) #f)))
                    (if input (parse input) '(exit))))
         (result (if command
                     (execute! command state-box)
                     'proceed)))
    (when (equal? 'proceed result)
      (user-loop! state-box))))
