#lang racket

(provide user-loop!
         run-execute!)

(require
 "../command/execute.rkt"
 "./render-list.rkt"
 "./parser.rkt"

 (prefix-in export: "../core/export.rkt")
 (prefix-in state: "../core/state.rkt")
 (prefix-in a: "../../attribute.rkt")
 (prefix-in term: "../../terminal.rkt"))

;; Print prompt and return input. Returns #f if user interrupts program while
;; waiting for input.
(define (prompt-line prompt)
  (display prompt)
  (with-handlers ((exn:break? (λ (e) #f)))
    (read-line)))

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

(define (prompt-proceed-unsaved state)
  (if (or (not (a:get (state state:state.open-file)))
          (and (file-exists? (a:get (state state:state.open-file)))
               (equal? (export:read-state-from-file (a:get (state state:state.open-file)))
                       state)))
      #t
      (begin
        (display "You have unsaved work. Proceed?  ")
        (let ((input (read-line)))
          (or (string-prefix? input "y")
              (string-prefix? input "Y"))))))

(define (run-execute! state-box command-line-representation)
  (define (handle! action)
    (match action
      (`(set-state ,new-state)
       (set-box! state-box new-state))
      (`(list-items ,item-data ,item)
       (displayln (render-listing item-data item)))
      (`(info-items ,item-data ,items)
       (displayln (render-listing-info item-data items)))
      (`(print-raw ,str)
       (displayln str))
      (`(error ,str)
       (displayln (term:render `((bold) (red) ("Error: " ,str)))))
      (`(write-file ,filename ,string-data)
       (call-with-output-file filename #:exists 'replace
         (λ (out)
           (display string-data out))))
      (`(load-state-from-file-and-then ,filename ,and-then)
       ;; If file does not exist, use empty state.
       (if (file-exists? filename)
           (let ((new-state (export:read-state-from-file filename)))
             (when (prompt-proceed-unsaved (unbox state-box))
               (handle! (list 'set-state new-state))
               (handle! (and-then new-state))))
           (begin (handle! (list 'set-state state:state-empty))
                  (handle! (and-then state:state-empty)))))))
  (map handle! (execute command-line-representation (unbox state-box)))
  (void))

(define (run-execute!/with-special state-box command-line-representation quit! proceed!)
  (match command-line-representation
    (`(parse-error)
     (displayln (term:render `((bold) (red) ("Error: Unable to parse command."))))
     (proceed!))
    (`(exit)
     (if (prompt-proceed-unsaved (unbox state-box))
         (quit!)
         (proceed!)))
    (else (begin (run-execute! state-box command-line-representation)
                 (proceed!)))))

(define (user-loop! state-box)
  (define (recur)
    (user-loop! state-box))
  (let* ((input (prompt-line (format-prompt-line (a:get ((unbox state-box) state:state.active-contexts)))))
         (parsed (try-parse input)))
    (run-execute!/with-special state-box
                               parsed
                               (lambda () (void))
                               recur)))
