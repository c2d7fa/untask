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

(define known-input/p
  (or/p known-keyword/p))

(define (concat/p . parsers)
  (f:map (lambda (results)
           (list results))
         (apply list/p parsers)))

(define input/p
  (concat/p (or/p (try/p known-input/p)
                  unknown/p)
            unknown/p))

(define (colorize input)
  (parse-result! (parse-string input/p input)))

