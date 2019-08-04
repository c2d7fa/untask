#lang racket

;; This module mostly just acts as a wrapper around the "gregor" library,
;; providing the functions that I'm interested in. It uses a different
;; representation for dates, which is easy to read and write from/to strings.

(provide (all-defined-out))

(require (prefix-in g: gregor))

(define (datetime year month day (hour #f) (minute #f))
  (list year month day hour minute))

(define (datetime-year dt) (list-ref dt 0))
(define (datetime-month dt) (list-ref dt 1))
(define (datetime-day dt) (list-ref dt 2))
(define (datetime-hour dt) (list-ref dt 3))
(define (datetime-minute dt) (list-ref dt 4))

(define (has-time? dt)
  (match dt
    (`(,y ,m ,d #f #f) #f)
    (else #t)))

(define (datetime->gregor dt)
  (match dt
    (`(,y ,m ,d #f #f) (g:date y m d))
    (`(,y ,m ,d ,h ,min) (g:datetime y m d h min))))

;; Return #t if the given datetime is in the future.
;;
;; Datetimes that do not contain a time component, are not in the future if they
;; are today. Datetimes that do contain a time component and whose day component
;; is the current day are in the future if and only if the time component is
;; later than the current time.
(define (future? dt)
  (if (has-time? dt)
      (g:datetime<? (g:now) (datetime->gregor dt))
      (g:date<? (g:today) (datetime->gregor dt))))

(define (today? dt)
  (g:date=? (g:today) (g:->date (datetime->gregor dt))))
