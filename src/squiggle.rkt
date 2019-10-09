#lang racket

;; TODO: We only really want to use λ~ inside of ~>; maybe we should just
;; combine the two?

(provide ~> ~>> λ> λ>> λ~)

(require (for-syntax racket/syntax))

(define-syntax (~> stx)
  (syntax-case stx ()
    ((_ x) #'x)
    ((_ x (f args ...) tail ...)
     #'(~> (f x args ...) tail ...))))

(define-syntax (~>> stx)
  (syntax-case stx ()
    ((_ x) #'x)
    ((_ x (f args ...) tail ...)
     #'(~>> (f args ... x) tail ...))))

(define-syntax-rule (λ> expr ...)
  (λ (x) (~> x expr ...)))

(define-syntax-rule (λ>> expr ...)
  (λ (x) (~>> x expr ...)))

(define-syntax (λ~ stx)
  (with-syntax ((~ (datum->syntax stx '~)))
    #`(λ (~) #,@(cdr (syntax-e stx)))))
