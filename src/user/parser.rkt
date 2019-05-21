#lang racket

(provide (all-defined-out))

(require
 (prefix-in expr: "../data/expressions.rkt")

 (combine-in megaparsack megaparsack/text)
 (prefix-in f: (combine-in data/functor data/applicative data/monad data/either))
 (only-in data/monad <-)
 )

(define digit/p (char-between/p #\0 #\9))
(define int/p (f:map (λ (ds) (string->number (apply string ds))) (many+/p digit/p)))

(define bare-word/p
  (let* ((valid-init/p (char-between/p #\a #\z))
         (valid-rest/p (or/p valid-init/p (char/p #\-) digit/p)))
    (f:do
     (c <- valid-init/p)
     (cs <- (many/p valid-rest/p))
     (f:pure (apply string c cs)))))

(define property-key/p
  (f:map (λ (cs) (string->symbol (apply string cs)))
         (many/p (char-between/p #\a #\z))))

(define literal-item-expression/p
  (f:map expr:make-item int/p))
(define literal-number-expression/p
  (f:map expr:make-number
         (f:do (char/p #\$)
               int/p)))  ; TODO: Parse non-integer numbers
(define literal-string-expression/p
  (f:map expr:make-string
         (or/p bare-word/p
               (f:do (char/p #\{)
                     (cs <- (many/p (char-not/p #\})))
                     (char/p #\})
                     (f:pure (apply string cs))))))

(define literal-expression/p
  (or/p literal-item-expression/p
        literal-number-expression/p
        literal-string-expression/p))

(define filter-or-modify-pair/p
  (let ((filter-or-modify-operator/p
         ;; TODO: Allow any valid operator.
         (f:map (λ (c) (string->symbol (string c)))
                (char-in/p ":<>+-/"))))  ; TODO: How to avoid hardcoding these?
    (f:do (key <- property-key/p)
          (op <- filter-or-modify-operator/p)
          (val <- literal-expression/p)
          (f:pure (list key op val)))))

(define whitespace/p (many+/p (char/p #\space)))
(define comma-whitespace/p (list/p (char/p #\,) whitespace/p))

(define (try-many-sep/p parser sep)
  (or/p (f:do (r <- parser)
              (rs <- (try/p (many/p (f:map second (try/p (list/p sep parser))))))
              (f:pure (cons r rs)))
        (list/p)))

(define filter-pair/p
  (or/p (f:do (char/p #\!)
              (fp <- filter-or-modify-pair/p)
              (f:pure (cons 'not fp)))
        filter-or-modify-pair/p
        literal-item-expression/p))

(define filter-expression/p
  (let* ((and-list/p (f:map (λ (subexprs) (cons 'and subexprs))
                            (try-many-sep/p filter-pair/p whitespace/p)))
         (or-list/p (f:map (λ (subexprs) (cons 'or subexprs))
                           (many/p and-list/p #:sep comma-whitespace/p))))
    or-list/p))

(define modify-expression/p
  (let* ((and-list/p (f:map (λ (subexprs) (cons 'and subexprs))
                            (many/p filter-or-modify-pair/p #:sep whitespace/p))))
    and-list/p))

(define command-line-input/p
  (or/p (try/p (f:do (fe <- filter-expression/p)
                     whitespace/p
                     (string/p "modify")
                     whitespace/p
                     (me <- modify-expression/p)
                     (f:pure (list fe 'modify me))))
        (try/p (f:do (fe <- filter-expression/p)
                     whitespace/p
                     (string/p "list")
                     (f:pure (list fe 'list))))
        (try/p (f:do (string/p "add")
                     whitespace/p
                     (me <- modify-expression/p)
                     (f:pure (list 'add me))))))

(define (parse command-line-input)
  (parse-result! (parse-string command-line-input/p command-line-input)))
