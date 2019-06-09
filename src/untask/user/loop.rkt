#lang racket

(provide user-loop!)

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
  (with-handlers ((exn? (λ (e) #f)))
    (parse input)))

;; Returns one of
;;  - 'exit
;;  - 'parse-error
;;  - `(success ,output ,new-state)
(define (try-evaluate input
                      state
                      #:property-types property-types)
  (if (or (eq? input #f)
          (equal? input "exit"))
      'exit
      (let ((parsed (try-parse input)))
        (if (eq? parsed #f)
            'parse-error
            (list 'success (execute parsed state #:property-types property-types))))))

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
          (equal? (export:read-state-from-file (a:get (state state:state.open-file)))
                  state))
      #t
      (begin
        (display "You have unsaved work. Proceed?  ")
        (let ((input (read-line)))
          (or (string-prefix? input "y")
              (string-prefix? input "Y"))))))

(define (user-loop! state-box
                    #:property-types property-types)
  (define (recur)
    (user-loop! state-box
                #:property-types property-types))
  (define (goodbye)
    (if (prompt-proceed-unsaved (unbox state-box))
        (displayln "Goodbye!")
        (recur)))
  (define (parse-error)
    (displayln "Error: Could not parse input.")
    (recur))
  (define (handle! action)
    (match action
      (`(set-state ,new-state)
       (set-box! state-box new-state))
      (`(list-items ,item-data ,item)
       (displayln (render-listing item-data item)))
      (`(print-raw ,str)
       (displayln str))
      (`(write-file ,filename ,string-data)
       (call-with-output-file filename #:exists 'replace
         (λ (out)
           (display string-data out))))
      (`(load-state-from-file-and-then ,filename ,and-then)
       (let ((new-state (export:read-state-from-file filename)))
         (when (prompt-proceed-unsaved (unbox state-box))
           (handle! (list 'set-state new-state))
           (handle! (and-then new-state)))))))
  (define (success output)
    (map handle! output)
    (recur))
  (let* ((input (prompt-line (format-prompt-line (a:get ((unbox state-box) state:state.active-contexts)))))
         (result (try-evaluate input
                               (unbox state-box)
                               #:property-types property-types)))
    (match result
      ('exit (goodbye))
      ('parse-error (parse-error))
      (`(success ,output) (success output)))))
