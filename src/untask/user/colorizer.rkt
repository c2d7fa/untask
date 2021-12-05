#lang racket

(provide colorize)

(require
 (combine-in megaparsack megaparsack/text)
 (prefix-in f: (combine-in data/functor data/applicative data/monad data/either))
 (only-in data/monad <-))

(define (concat/p . parsers)
  (f:map (lambda (results)
           (list results))
         (apply list/p parsers)))

(define (colorized/p colors p)
  (f:map (lambda (result)
           `(,@colors (,result)))
         p))

(define (keyword/p keyword)
  (colorized/p '((bold)) (string/p keyword)))

(define keywords '("add" "list" "exit" "context" "remove" "modify" "copy" "save" "open" "info" "agenda" "schedule" "tree"))
(define known-keyword/p
  (apply or/p (map keyword/p keywords)))

(define operators '(":" "/" "<" ">" "+" "-"))
(define known-operator/p
  (apply or/p (map keyword/p operators)))

(define unknown/p
  (f:map (lambda (chars)
           `((white) (,(apply string chars))))
         (many/p any-char/p)))

(define invalid/p
  (f:map (lambda (chars)
           `((underline) (red) (,(apply string chars))))
         (many/p any-char/p)))

(define whitespace/p
  (f:map (lambda (chars)
           `((white) (,(apply string chars))))
         (many+/p (char-in/p " ,"))))

(define (standalone/p p)
  (f:do (result <- p)
        (lookahead/p (or/p (try/p whitespace/p)
                           eof/p))
        (f:pure result)))

(define incomplete-curly-string/p
  (f:do (string/p "{")
        [body <- (many/p (char-not-in/p "}"))]
        (f:pure
          `((((bright black) ("{"))
             ((green) (underline) (,(apply string body))))))))

(define complete-curly-string/p
  (f:do (string/p "{")
        [body <- (many/p (char-not-in/p "}"))]
        (string/p "}")
        (f:pure
          `((((bright black) ("{"))
             ((green) (,(apply string body)))
             ((bright black) ("}")))))))

(define bareword-string/p
  (standalone/p
    (f:map (lambda (chars)
             `((green) (,(apply string chars))))
           (many+/p (char-between/p #\a #\z)))))

(define string-literal/p
  (or/p (try/p complete-curly-string/p)
        (try/p incomplete-curly-string/p)
        (try/p bareword-string/p)))

(define incomplete-number-literal/p
  (colorized/p '((underline) (white)) (string/p "$")))

(define (chars->string/p p)
  (f:map (lambda (chars)
           (apply string chars))
         p))

(define complete-number-literal/p
  (concat/p (colorized/p '((black)) (string/p "$"))
            (colorized/p '((magenta)) (chars->string/p (many+/p (char-in/p "0123456789"))))))

(define number-literal/p
  (or/p (try/p complete-number-literal/p)
        (try/p incomplete-number-literal/p)))

(define (any-prefix-of/p s)
  (guard/p (chars->string/p (many/p (char-in/p s)))
           (lambda (r)
             (string-prefix? s r))))

(define date-literal/p
  (or/p (try/p (colorized/p '((yellow)) (string/p "Today")))
        (try/p (colorized/p '((yellow) (underline)) (any-prefix-of/p "Today")))))

(define literal/p
  (or/p (try/p number-literal/p)
        (try/p string-literal/p)
        (try/p date-literal/p)))

(define property-name/p
  (colorized/p '((blue)) (chars->string/p (many+/p (char-between/p #\a #\z)))))

(define tag-expression/p
  (let ((hash/p (or/p (try/p (colorized/p '((blue)) (string/p "-#")))
                      (try/p (colorized/p '((blue)) (string/p "#"))))))
    (or/p (try/p (concat/p hash/p bareword-string/p))
          (try/p (colorized/p '((underline)) hash/p)))))

(define prefix-operators/p
  (colorized/p '((bold)) (chars->string/p (many/p (char-in/p "!")))))

(define expression/p
  (standalone/p
    (or/p
      (try/p tag-expression/p)
      (try/p (concat/p known-operator/p literal/p))
      (try/p (concat/p prefix-operators/p property-name/p known-operator/p literal/p))
      (try/p (colorized/p '((underline)) (concat/p prefix-operators/p property-name/p known-operator/p)))
      (try/p (colorized/p '((underline)) (concat/p prefix-operators/p property-name/p)))
      (try/p complete-curly-string/p)
      (try/p incomplete-curly-string/p))))

(define token/p
  (or/p (try/p known-keyword/p)
        (try/p expression/p)
        (try/p (f:map (lambda (chars)
                        `((white) (,(apply string chars))))
                      (many+/p (char-not-in/p " ,"))))))

(define (sep/p main sep)
  (many/p (or/p (try/p (f:map (lambda (results)
                                (list results))
                              (list/p
                                (f:do [x <- main]
                                      (lookahead/p sep)
                                      (f:pure x))
                                sep)))
                (f:do [x <- main]
                      (lookahead/p eof/p)
                      (f:pure x)))))

(define input/p
  (f:map (lambda (results)
           (list results))
         (sep/p token/p whitespace/p)))

(define (colorize input)
  (parse-result! (parse-string input/p input)))

(require "../../terminal.rkt")
(display! (colorize "dat"))
(displayln "")
(display! (colorize "date:"))
(displayln "")
(display! (colorize "date:Tod"))
(displayln "")
(display! (colorize "date:Today modify #tag description:{some string} urgency:$10 /{hello}"))

