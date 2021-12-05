#lang racket

(provide colorize)

(require
 (combine-in megaparsack megaparsack/text)
 (prefix-in f: (combine-in data/functor data/applicative data/monad data/either))
 (only-in data/monad <-))

(define (keyword/p keyword)
  (f:map (lambda (string)
           `((bold) (,string)))
         (string/p keyword)))

(define keywords '("add" "list" "exit" "context" "remove" "modify" "copy" "save" "open" "info" "agenda" "schedule" "tree"))
(define known-keyword/p
  (apply or/p (map keyword/p keywords)))

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
         (many+/p (char-in/p " "))))

(define (standalone/p p)
  (f:do (result <- p)
        (lookahead/p (or/p (try/p whitespace/p)
                           eof/p))
        (f:pure result)))

(define incomplete-curly-string/p
  (f:do (string/p "{")
        [body <- (many/p (char-not-in/p "}"))]
        (f:pure
          `((((white) ("{"))
             ((green) (underline) (italic) (,(apply string body))))))))

(define complete-curly-string/p
  (f:do (string/p "{")
        [body <- (many/p (char-not-in/p "}"))]
        (string/p "}")
        (f:pure
          `((((white) ("{"))
             ((green) (italic) (,(apply string body)))
             ((white) ("}")))))))

(define bareword-string/p
  (standalone/p
    (f:map (lambda (chars)
             `((green) (italic) (,(apply string chars))))
           (many+/p (char-between/p #\a #\z)))))

(define string-literal/p
  (or/p (try/p complete-curly-string/p)
        (try/p incomplete-curly-string/p)
        (try/p bareword-string/p)))


(define token/p
  (or/p (try/p known-keyword/p)
        (try/p string-literal/p)
        (try/p (f:map (lambda (chars)
                        `((white) (,(apply string chars))))
                      (many+/p (char-not-in/p " "))))))

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

