#lang racket

(define (initialize input)
  `#hash((input . ,input)
         (position . 0)))

(define (success state value)
  `#hash((success? . #t)
         (value . ,value)
         (state . ,state)))

(define (failure state reason)
  `#hash((success? . #f)
         (reason . ,reason)
         (state . ,state)))

(define (success? result)
  (hash-ref result 'success?))

(define (value result)
  (hash-ref result 'value))

(define (reason result)
  (hash-ref result 'reason))

(define (state result)
  (hash-ref result 'state))

(define (map-result f result)
  (if (success? result)
    `#hash((success? . #t)
           (value . ,(f (value result)))
           (state . ,(state result)))
    result))

(define (peek-char s)
  (let ((input (hash-ref s 'input)))
    (if (< (hash-ref s 'position) (string-length input))
        (string-ref input (hash-ref s 'position))
      #f)))

(define (move-char s offset)
  (hash-set s 'position (+ (hash-ref s 'position) offset)))

(define <empty>
  (lambda (s)
    (success s #f)))

(define <any-char>
  (lambda (s)
    (let ((char (peek-char s)))
      (success (move-char s 1)
               char))))

(define (<map f <p>)
  (lambda (s)
    (let ((result (<p> s)))
      (map-result f result))))

(define (<reason <p> reason)
  (lambda (s)
    (let ((result (<p> s)))
      (if (success? result)
        result
        (failure (state result) reason)))))

(define (<where <p> pred)
  (lambda (s)
    (let ((result (<p> s)))
      (if (success? result)
        (if (pred (value result))
          result
          (failure (state result) "Subparser did not match predicate"))
        result))))

(define (<some ps)
  (lambda (s)
    (if (null? ps)
      (failure s "No subparsers")
      (let ((result ((car ps) s)))
        (if (success? result)
          result
          ((<some (cdr ps)) s))))))

(define (<list ps)
  (lambda (s)
    (if (null? ps)
      (success s '())
      (let ((result ((car ps) s)))
        (if (success? result)
          (map-result (lambda (values) (cons (value result) values))
                      ((<list (cdr ps)) (state result)))
          (failure s (format "Subparser did not match list: ~a" (reason result))))))))

(define (<char c)
  (<reason
    (<where <any-char> (lambda (char) (equal? c char)))
    (format "Expected character: ~a" c)))

(define (<string str)
  (<map (lambda (chars) (apply string chars))
        (<list (map <char (string->list str)))))

(writeln ((<some (list (<string "test123")
                       (<string "test100")))
          (initialize "test123 this is")))

(displayln "done")

