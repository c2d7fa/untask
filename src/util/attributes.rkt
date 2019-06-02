#lang racket

(define (make-attribute #:get getter #:set setter)
  (list* getter setter))

;;

(define .car
  (make-attribute #:get (λ (x) (car x))
                  #:set (λ (x v)
                          (cons v (cdr x)))))

(define .cdr
  (make-attribute #:get (λ (x) (cdr x))
                  #:set (λ (x v) (cons (car x) v))))

(define (.nth n)
  (apply path (append (make-list n .cdr) (list .car))))

;;

(define (path . attrs)
  (make-attribute #:get (λ (x)
                          (get* x attrs))
                  #:set (λ (x v)
                          (set* x attrs v))))

(define (get* head path)
  (foldl (λ (attr x)
           ((car attr) x))
         head
         path))

(define (set* head path v)
  (if (empty? path)
      v
      ((cdr (car path))
       head
       (set* ((car (car path)) head)
             (cdr path)
             v))))

(define (update* head path f)
  (set* head path (f (get* head path))))

(define-syntax-rule (get (head path ...))      (get* head (list path ...)))
(define-syntax-rule (set (head path ...) v)    (set* head (list path ...) v))
(define-syntax-rule (update (head path ...) f) (update* head (list path ...) f))

;;

(require (for-syntax racket/syntax
                     racket/list))

(define-syntax (define-record stx)
  (syntax-case stx ()
    ((_ name (attrs ...))
     (with-syntax ((make-name (format-id #'name "~a" #'name))
                   ((make-name-args ...) (apply append (map (λ (x)
                                                        (list (string->keyword (symbol->string (syntax->datum x)))
                                                              x))
                                                      (syntax-e #'(attrs ...)))))
                   (make-name-body #'(list attrs ...))
                   ((define-attrs ...) (map (λ (n)
                                              (with-syntax ((n (datum->syntax #'name n))
                                                            (attr-name (format-id #'name "~a.~a" #'name (list-ref (syntax-e #'(attrs ...)) n))))
                                                #'(define attr-name
                                                    (.nth n))))
                                            (range (length (syntax-e #'(attrs ...))))))
                   )
       #'(begin
           (define (make-name make-name-args ...)
             make-name-body)
           define-attrs ...)))))

(define-syntax-rule (define-attribute name #:get getter #:set setter)
  (define name (make-attribute #:get getter #:set setter)))
