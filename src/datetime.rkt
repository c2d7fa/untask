#lang racket

;; This module mostly just acts as a wrapper around the "gregor" library,
;; providing the functions that I'm interested in. It uses a different
;; representation for dates, which is easy to read and write from/to strings.

(provide (all-defined-out))

(require (prefix-in g: gregor)
         (prefix-in gp: gregor/period))

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

(define (gregor->datetime gdt)
  (if (g:datetime? gdt)
      (datetime (g:->year gdt) (g:->month gdt) (g:->day gdt) (g:->hours gdt) (g:->minutes gdt))
      (datetime (g:->year gdt) (g:->month gdt) (g:->day gdt))))

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

(define (today)
  (gregor->datetime (g:today)))

(define (today? dt)
  (g:date=? (g:today) (g:->date (datetime->gregor dt))))

(define (days-between dt1 dt2)
  (cdr (assoc 'days (gp:period->list (gp:date-period-between (datetime->gregor dt1) (datetime->gregor dt2) '(days))))))

(define (days-from-today dt)
  (cdr (assoc 'days (gp:period->list (gp:date-period-between (g:today) (datetime->gregor dt) '(days))))))

(define (drop-time dt)
  (datetime (datetime-year dt)
            (datetime-month dt)
            (datetime-day dt)))

(define (same-date? dt1 dt2)
  (equal? (drop-time dt1) (drop-time dt2)))

(define (date-before? dt1 dt2)
  (g:date<? (datetime->gregor (drop-time dt1))
            (datetime->gregor (drop-time dt2))))

(define (before? dt1 dt2)
  (if (and (has-time? dt1) (has-time? dt2))
      (g:datetime<? (datetime->gregor dt1)
                    (datetime->gregor dt2))
      (g:date<? (datetime->gregor (drop-time dt1))
                (datetime->gregor (drop-time dt2)))))

(define (after? dt1 dt2)
  (if (and (has-time? dt1) (has-time? dt2))
      (g:datetime>? (datetime->gregor dt1)
                    (datetime->gregor dt2))
      (g:date>? (datetime->gregor (drop-time dt1))
                (datetime->gregor (drop-time dt2)))))

(define (weekday-short-string dt)
  (case (g:->wday (datetime->gregor dt))
    ((0) "Sun")
    ((1) "Mon")
    ((2) "Tue")
    ((3) "Wed")
    ((4) "Thu")
    ((5) "Fri")
    ((6) "Sat")))

(define (month-short-string dt)
  (case (g:->month (datetime->gregor dt))
    ((1) "Jan")
    ((2) "Feb")
    ((3) "Mar")
    ((4) "Apr")
    ((5) "May")
    ((6) "Jun")
    ((7) "Jul")
    ((8) "Aug")
    ((9) "Sep")
    ((10) "Oct")
    ((11) "Nov")
    ((12) "Dec")))

(define (this-year? dt)
  (equal? (g:->year (g:today))
          (g:->year (datetime->gregor dt))))

(define (current-year)
  (g:->year (g:today)))

(define (is-valid-date? dt)
  (<= (datetime-day dt) (g:days-in-month (datetime-year dt) (datetime-month dt))))

(define (add-days dt n)
  (gregor->datetime (g:+days (datetime->gregor dt) n)))

(define (date-range start end skip)
  (if (< (days-between start end) skip)
      (list start)
      (cons start (date-range (add-days start skip) end skip))))

;; Format the date like "Tue 2019-Nov-05"
(define (format-full-date-weekday dt)
  (format "~a ~a-~a-~a"
          (weekday-short-string dt)
          (g:->year (datetime->gregor dt))
          (month-short-string dt)
          (~r (g:->day (datetime->gregor dt)) #:min-width 2 #:pad-string "0")))
