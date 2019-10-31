#lang racket

(provide (all-defined-out))

(require
 (prefix-in dt: untask/src/datetime))

(define (make-string string-content)
  `(string . ,string-content))
(define (make-set (set-value (set)))
  `(set . ,set-value))
(define (make-number x)
  `(number . ,x))
(define (make-item id)
  `(item . ,id))
(define (make-boolean v)
  `(boolean . ,v))
(define (make-date datetime)
  `(date . ,datetime))

(define unwrap-string cdr)
(define unwrap-set cdr)
(define unwrap-number cdr)
(define unwrap-item cdr)
(define unwrap-boolean cdr)
(define unwrap-date cdr)

(define (string-value? x) (and x (pair? x) (eq? 'string (car x))))
(define (set-value? x) (and x (pair? x) (eq? 'set (car x))))
(define (number-value? x) (and x (pair? x) (eq? 'number (car x))))
(define (item-value? x) (and x (pair? x) (eq? 'item (car x))))
(define (boolean-value? x) (and x (pair? x) (eq? 'boolean (car x))))
(define (date-value? x) (and x (pair? x) (eq? 'date (car x))))
(define (empty-value? x) (not x))

(define (get-type v)
  (if (eq? #f v)
      'none
      (let ((type (car v)))
        (if (eq? type 'set)
            (if (set-empty? (cdr v))
                '(set any)
                `(set ,(get-type (car (set->list (cdr v))))))
            type))))

(define (type<=? t1 t2)
  (or (equal? t1 t2)
      (eq? 'any t1)
      (eq? 'any t2)
      (and (list? t1)
           (list? t2)
           (type<=? (cadr t1)
                    (cadr t2)))
      (and (list? t2)
           (eq? 'opt (car t2))
           (or (eq? 'none t1)
               (type<=? t1 (cadr t2))))))

(define (has-type? x t)
  (or ;; Direct subtype
      (type<=? (get-type x) t)
      ;; Optional
      (and (list? t)
           (eq? 'opt (car t))
           (or (eq? #f x)
               (has-type? x (cadr t))))
      ;; Enums
      (and (list? t)
           (eq? 'enum (car t))
           (member (string->symbol (unwrap-string x)) (cdr t)))))

(define (evaluate-literal literal-expression)
  (match literal-expression
    (`(string . ,string-content) `(string . ,string-content))
    (`(number . ,number-value) `(number . ,number-value))
    (`(item . ,item-id) `(item . ,item-id))
    (`(set . ,set-expressions) `(set . ,(list->set (map evaluate-literal set-expressions))))
    (`(boolean . ,boolean-value) (make-boolean boolean-value))
    (`(date ,year ,month ,day) (make-date (dt:datetime (or year (dt:current-year)) month day)))
    (`(date ,year ,month ,day ,hours ,minutes) (make-date (dt:datetime (or year (dt:current-year)) month day hours minutes)))
    (`(date today ,offset) (make-date (dt:add-days (dt:today) offset)))
    (#f #f)))

