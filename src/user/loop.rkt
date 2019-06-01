#lang racket

(provide user-loop!)

(require
 "execute.rkt"
 (prefix-in state: "../data/state.rkt"))

;; Print prompt and return input. Returns #f if user interrupts program while
;; waiting for input.
(define (prompt-line prompt)
  (display prompt)
  (with-handlers ((exn:break? (λ (e) #f)))
    (read-line)))

(define (try-parse input #:parse parse)
  (with-handlers ((exn? (λ (e) #f)))
    (parse input)))

;; Returns one of
;;  - 'exit
;;  - 'parse-error
;;  - `(success ,output ,new-state)
(define (try-evaluate input
                      state
                      #:parse parse
                      #:property-types property-types)
  (if (or (eq? input #f)
          (equal? input "exit"))
      'exit
      (let ((parsed (try-parse input #:parse parse)))
        (if (eq? parsed #f)
            'parse-error
            (let-values (((new-state output)
                          ((execute parsed
                                    #:property-types property-types)
                           state)))
              (list 'success output new-state))))))

(define (format-prompt-line current-contexts)
  (format "~a> " (string-join (map (λ (c) (format "@~a" c))
                                   (set->list current-contexts))
                              " ")))

(define (user-loop! state-box
                    #:parse parse
                    #:render-listing render-listing
                    #:property-types property-types)
  (define (recur)
    (user-loop! state-box
                #:parse parse
                #:render-listing render-listing
                #:property-types property-types))
  (define (goodbye)
    (displayln "Goodbye!"))
  (define (parse-error)
    (displayln "Error: Could not parse input.")
    (recur))
  (define (success output new-state)
    (displayln (render-listing (state:state-item-data new-state) output))
    (set-box! state-box new-state)
    (recur))
  (let* ((input (prompt-line (format-prompt-line (state:state-current-contexts (unbox state-box)))))
         (result (try-evaluate input
                               (unbox state-box)
                               #:property-types property-types
                               #:parse parse)))
    (match result
      ('exit (goodbye))
      ('parse-error (parse-error))
      (`(success ,output ,new-state) (success output new-state)))))
