#lang racket

(provide user-loop!)

(require
 untask/src/untask/user/render-list
 untask/src/untask/user/parser
 untask/src/untask/user/execute

 untask/src/untask/core/state
 (prefix-in c: untask/src/untask/core/context)
 (prefix-in a: untask/src/attribute)
 (prefix-in term: untask/src/terminal)

 (only-in untask/src/misc try-read-line*)
 untask/src/squiggle)

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
