#lang racket

(provide user-loop!)

(require
 "execute.rkt")

(define (prompt-line prompt)
  (display prompt)
  (read-line))

(define (user-loop! item-data-box #:parse parse #:render-listing render-listing)
  (define (goodbye)
    (displayln "Goodbye!"))
  (define (parse-error)
    (displayln "Error: Could not parse input.")
    (user-loop! item-data-box #:parse parse #:render-listing render-listing))
  (let ((input (with-handlers ((exn:break? (λ (e) #f)))
                 (prompt-line "> "))))
    (if (or (eq? input #f) (equal? input "exit"))
        (goodbye)
        (let ((parsed (with-handlers ((exn? (λ (e) #f)))
                        (parse input))))
          (if (eq? parsed #f)
              (parse-error)
              (let-values (((new-item-data output) ((execute parsed) (unbox item-data-box))))
                (set-box! item-data-box new-item-data)
                (displayln (render-listing new-item-data output))
                (user-loop! item-data-box #:parse parse #:render-listing render-listing)))))))

