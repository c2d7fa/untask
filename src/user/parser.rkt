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

(define literal-set-expression/p
  (f:map expr:make-set
         (f:do (string/p "[")
               (element-exprs <- (many/p literal-expression/p #:sep whitespace/p))
               (string/p "]")
               (f:pure element-exprs))))

(define literal-expression/p
  (or/p literal-set-expression/p
        literal-item-expression/p
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
              (f:pure (list 'not fp)))
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

(define (opt/p parser #:default (default (void)))
  (or/p (try/p parser)
        (f:pure default)))

(define (command/p command-name #:takes-filter? (takes-filter? #t) #:arguments (arguments/p #f))
  (f:do (fe <- (if takes-filter?
                   (opt/p (f:do (e <- filter-expression/p)
                                whitespace/p
                                (f:pure (list e)))
                          #:default (list '(and)))
                   (f:pure (list))))
        (string/p (symbol->string command-name))
        (args <- (if arguments/p
                     (f:do whitespace/p
                           (args <- arguments/p)
                           (f:pure args))
                     (f:pure (list))))
        (f:pure (append fe (list command-name) args))))

(define filename/p
  (f:map (λ (cs) (apply string cs))
         (many/p any-char/p)))

(define command-line-input/p
  (or/p (try/p (command/p 'modify #:arguments (list/p modify-expression/p)))
        (try/p (command/p 'list))
        (try/p (command/p 'add #:takes-filter? #f #:arguments (list/p modify-expression/p)))
        (try/p (command/p 'save #:takes-filter? #f #:arguments (list/p filename/p)))
        (try/p (command/p 'load #:takes-filter? #f #:arguments (list/p filename/p)))
        (f:map (λ (fe) `(,fe list)) filter-expression/p)))

(define (parse command-line-input)
  (parse-result! (parse-string command-line-input/p command-line-input)))
