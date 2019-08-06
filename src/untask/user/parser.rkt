#lang racket

(provide (all-defined-out))

(require
 (prefix-in expr: "../command/expression.rkt")

 (combine-in megaparsack megaparsack/text)
 (prefix-in f: (combine-in data/functor data/applicative data/monad data/either))
 (only-in data/monad <-))

(define digit/p (char-between/p #\0 #\9))
(define int/p (f:map (λ (ds) (string->number (apply string ds))) (many+/p digit/p)))
(define (range/p low high) (guard/p int/p (λ (x) (<= low x high))))
(define number/p
  (let ((float/p (f:do (a <- int/p)
                       (string/p ".")
                       (b <- int/p)
                       (f:pure (string->number (format "~a.~a" a b))))))
    (or/p (try/p float/p)
          int/p)))

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
               number/p)))

(define bracketed-string/p
  (f:do (char/p #\{)
        (cs <- (many/p (char-not/p #\})))
        (char/p #\})
        (f:pure (apply string cs))))

(define literal-string-expression/p
  (f:map expr:make-string
         (or/p bare-word/p bracketed-string/p)))

(define literal-set-expression/p
  (f:map expr:make-set
         (f:do (string/p "[")
               (element-exprs <- (many/p literal-expression/p #:sep whitespace/p))
               (string/p "]")
               (f:pure element-exprs))))

(define literal-date-expression/p
  (let* ((year/p int/p)
         (month/p (or/p (range/p 1 12)
                        (or/p (f:map (λ (_) 1) (try/p (string/p "Jan")))
                              (f:map (λ (_) 2) (try/p (string/p "Feb")))
                              (f:map (λ (_) 3) (try/p (string/p "Mar")))
                              (f:map (λ (_) 4) (try/p (string/p "Apr")))
                              (f:map (λ (_) 5) (try/p (string/p "May")))
                              (f:map (λ (_) 6) (try/p (string/p "Jun")))
                              (f:map (λ (_) 7) (try/p (string/p "Jul")))
                              (f:map (λ (_) 8) (try/p (string/p "Aug")))
                              (f:map (λ (_) 9) (try/p (string/p "Sep")))
                              (f:map (λ (_) 10) (try/p (string/p "Oct")))
                              (f:map (λ (_) 11) (try/p (string/p "Nov")))
                              (f:map (λ (_) 12) (try/p (string/p "Dec"))))))
         (day/p (range/p 1 31))
         (hour/p (range/p 0 23))
         (minute/p (range/p 0 59))
         (date-part/p
          (or/p (try/p (f:do (year <- year/p)
                             (string/p "-")
                             (month <- month/p)
                             (string/p "-")
                             (day <- day/p)
                             (f:pure (list year month day))))
                (try/p (f:do (month <- month/p)
                             (string/p "-")
                             (day <- day/p)
                             (f:pure (list #f month day)))))))
    (or/p (try/p (f:do (date-part <- date-part/p)
                       (string/p "T")
                       (hour <- hour/p)
                       (string/p ":")
                       (minute <- minute/p)
                       (f:pure `(date ,@date-part ,hour ,minute))))
          (f:do (date-part <- date-part/p)
                (f:pure `(date ,@date-part))))))

(define literal-expression/p
  (or/p literal-set-expression/p
        (try/p literal-date-expression/p)
        literal-item-expression/p
        literal-number-expression/p
        literal-string-expression/p))

(define filter-or-modify-pair-abbreviation/p
  (let ((description-pair/p (f:map (λ (v)
                                     `(description : (string . ,v)))
                                   bracketed-string/p))
        (tags+-pair/p (f:do (string/p "#")
                            (t <- bare-word/p)
                            (f:pure `(tags + (string . ,t)))))
        (tags--pair/p (f:do (string/p "-#")
                            (t <- bare-word/p)
                            (f:pure `(tags - (string . ,t))))))
    (or/p (try/p description-pair/p)
          (try/p tags+-pair/p)
          (try/p tags--pair/p))))


(define valid-operator-names ":<>+-/")

(define filter-or-modify-pair/p
  (or/p filter-or-modify-pair-abbreviation/p
        (let ((filter-or-modify-operator/p
               (f:map (λ (c) (string->symbol (string c)))
                      (char-in/p valid-operator-names))))
          (or/p (try/p
                 (f:do (key <- property-key/p)
                       (op <- filter-or-modify-operator/p)
                       (val <- literal-expression/p)
                       (f:pure (list key op val))))
                (f:do (key <- property-key/p)
                      (string/p ":")
                      (f:pure (list key ': #f)))))))

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
        (f:do (string/p "/")
              (s <- literal-string-expression/p)
              (f:pure `(description / ,s)))
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

(define (normal-command/p command-name #:takes-filter? (takes-filter? #t) #:arguments (arguments/p #f))
  (f:do (fe <- (if takes-filter?
                   (opt/p (f:do (e <- filter-expression/p)
                                whitespace/p
                                (f:pure (list e)))
                          #:default (list '(and)))
                   (f:pure (list))))
        (string/p (symbol->string command-name))
        (args <- (if arguments/p
                     (f:do (opt/p whitespace/p)
                           (args <- arguments/p)
                           (f:pure args))
                     (f:pure (list))))
        (f:pure (append fe (list command-name) args))))

(define context-name/p bare-word/p)

(define context-toggles/p
  (let ((toggle/p (or/p (try/p (f:do (string/p "@")
                                     (name <- context-name/p)
                                     (f:pure `(on ,name))))
                        (f:do (string/p "-@")
                              (name <- context-name/p)
                              (f:pure `(off ,name)))
                        (f:do (string/p "@")
                              (f:pure '(reset))))))
    (f:do (x <- toggle/p)
          (xs <- (many/p
                  (f:map cadr
                         ;; We use this instead of (many+/p toggle/p #:sep
                         ;; whitespace/p), beucase we need this try/p here for it to
                         ;; work with commands like "@context list".
                         (try/p (list/p whitespace/p toggle/p)))))
          (f:pure `(,x ,@xs)))))

(define context-command/p
  (let* ((context-list-command/p
          (f:do (string/p "context")
                whitespace/p
                (string/p "list")
                (f:pure '(context show))))
         (context-add-command/p
          (f:do (string/p "context")
                whitespace/p
                (string/p "add")
                whitespace/p
                (name <- context-name/p)
                (fe <- (opt/p #:default '()
                              (f:do whitespace/p
                                    (string/p "filter")
                                    whitespace/p
                                    (fe <- filter-expression/p)
                                    (f:pure fe))))
                (me <- (opt/p #:default '()
                              (f:do whitespace/p
                                    (string/p "modify")
                                    whitespace/p
                                    (me <- modify-expression/p)
                                    (f:pure me))))
                (f:pure `(context add ,name ,fe ,me))))
         (context-remove-command/p
          (f:do (string/p "context")
                whitespace/p
                (string/p "remove")
                whitespace/p
                (name <- context-name/p)
                (f:pure `(context remove ,name))))
         (context-set-active-command/p
          (f:do (toggles <- context-toggles/p)
                (f:pure `(context active ,toggles)))))
    (or/p (try/p context-list-command/p)
          (try/p context-add-command/p)
          (try/p context-remove-command/p)
          (try/p context-set-active-command/p)
          )))

(define filename/p
  (f:map (λ (cs) (apply string cs))
         (many/p any-char/p)))

(define (with-context/p p)
  (or/p
   (try/p (f:do (toggles <- context-toggles/p)
                whitespace/p
                (subcommand <- p)
                (f:pure `(with-contexts ,toggles ,subcommand))))
   p))

(define command-line-input/p
  (or/p
   (try/p (with-context/p (normal-command/p 'modify #:arguments (list/p modify-expression/p))))
   (try/p (with-context/p (normal-command/p 'list)))
   (try/p (with-context/p (normal-command/p 'add #:takes-filter? #f #:arguments (list/p modify-expression/p))))
   (try/p (normal-command/p 'save #:takes-filter? #f))
   (try/p (normal-command/p 'open #:takes-filter? #f #:arguments (list/p filename/p)))
   (try/p (with-context/p (normal-command/p 'remove #:takes-filter? #t)))
   (try/p (with-context/p (normal-command/p 'info #:takes-filter? #t)))
   (try/p (normal-command/p 'exit #:takes-filter? #f))
   (try/p (with-context/p (normal-command/p 'agenda)))
   (try/p (with-context/p (normal-command/p 'tree #:arguments (opt/p (list/p filter-expression/p) #:default '(())))))
   (try/p (with-context/p (f:map (λ (fe) `(,fe list)) filter-expression/p)))
   context-command/p))

(define (parse command-line-input)
  (parse-result! (parse-string (f:do (result <- command-line-input/p)
                                     (opt/p whitespace/p)
                                     eof/p
                                     (f:pure result))
                               command-line-input)))
