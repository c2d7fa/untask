#lang racket

(provide user-loop!)

(require
 "render-list.rkt"
 "parser.rkt"
 "run.rkt"
 "../command/interpret.rkt"

 (prefix-in export: "../core/export.rkt")
 (prefix-in state: "../core/state.rkt")
 (prefix-in a: "../../attribute.rkt")
 (prefix-in term: "../../terminal.rkt")

 (only-in "../../misc.rkt" try-read-line*))

;; Print prompt and return input. Returns #f if user interrupts program while
;; waiting for input.
(define (prompt-line prompt)
  (display prompt)
  (try-read-line*))

(define (try-parse input)
  (with-handlers ((exn? (λ (e) '(parse-error))))
    (if (not input) '(exit) (parse input))))

(define (format-prompt-line current-contexts)
  (let ((contexts (string-join
                   (map (λ (c)
                          (term:render
                           `(() (((black) ("@"))
                                 ((cyan) (,c))))))
                        (set->list current-contexts))
                   " ")))
    (term:render `(()
                   (,contexts
                    ((black) ("> ")))))))

(define (user-loop! state-box)
  (define (recur)
    (user-loop! state-box))
  (let* ((input (prompt-line (format-prompt-line (a:get ((unbox state-box) state:state.active-contexts)))))
         (command (with-handlers ((exn:fail:read? (λ (e) (displayln (term:render '((bold) (red) ("Error: Unable to parse command.")))) #f)))
                    (if input (parse input) '(exit))))
         (result (if command
                     (execute! command state-box)
                     'proceed)))
    (when (equal? 'proceed result)
      (user-loop! state-box))))
