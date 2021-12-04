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
 (prefix-in line: "../../line-editor.rkt")

 "../../squiggle.rkt")

(define line-editor-box (box line:line-editor-empty))

(define (read-line-raw!)
  (define (continue-reading!)
    (term:display! (line:output (unbox line-editor-box)))
    (let* ((c (read-char)))
      (let-values (((value state) (line:accept (unbox line-editor-box) c)))
        (set-box! line-editor-box state)
        (if value
          (begin (term:display! '(("\n")))
                 value)
          (continue-reading!)))))
  (continue-reading!))

;; Print prompt and return input. Returns #f if user interrupts program while
;; waiting for input.
(define (prompt-line prompt)
  (term:with-raw! (read-line-raw!)))

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
