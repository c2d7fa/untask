#lang racket

(provide user-loop!)

(require
 "execute.rkt")

(define (prompt-line prompt)
  (display prompt)
  (read-line))

(define (user-loop! item-data-box #:parse parse #:render-listing render-listing #:property-types property-types)
  (define (recur)
    (user-loop! item-data-box #:parse parse #:render-listing render-listing #:property-types property-types))
  (define (goodbye)
    (displayln "Goodbye!"))
  (define (parse-error)
    (displayln "Error: Could not parse input.")
    (recur))
  (let ((input (with-handlers ((exn:break? (λ (e) #f)))
                 (prompt-line "> "))))
    (if (or (eq? input #f) (equal? input "exit"))
        (goodbye)
        (let ((parsed (with-handlers ((exn? (λ (e) #f)))
                        (parse input))))
          (if (eq? parsed #f)
              (parse-error)
              (let-values (((new-item-data output) ((execute parsed #:property-types property-types) (unbox item-data-box))))
                (set-box! item-data-box new-item-data)
                (displayln (render-listing new-item-data output))
                (recur)))))))

