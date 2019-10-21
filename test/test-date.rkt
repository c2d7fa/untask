#lang racket

(require rackunit
         untask/src/squiggle

         untask/src/untask/properties/date

         (prefix-in i: untask/src/untask/core/item)
         (prefix-in v: untask/src/untask/core/value)
         (prefix-in dt: untask/src/datetime))

(provide date-tests)

(define (set-disjoint? s1 s2)
  (set-empty? (set-intersect s1 s2)))

(define-syntax-rule (new-item! item-state-var)
  (let-values (((state* item*) (i:new item-state-var)))
    (set! item-state-var state*)
    item*))

;; Example state with three items.
;;
;; - st1/1 has date and wait date with two days difference.
;; - st1/2 has date but no wait date
;; - st1/3 has neither date nor wait date
(define-values (st1 it1 it2 it3)
  (let ((st1 i:empty-state))
    (define it1 (new-item! st1))
    (define it2 (new-item! st1))
    (define it3 (new-item! st1))
    (~>! st1
         (i:set it1 'description (v:make-string "Item 1"))
         (i:set it1 'tags (v:make-set (set (v:make-string "tag1"))))
         (i:set it2 'description (v:make-string "Item 2"))
         (i:set it3 'description (v:make-string "Item 3"))
         (i:set it1 'date (v:make-date (dt:datetime 2018 1 2)))
         (i:set it1 'wait (v:make-date (dt:datetime 2018 1 4)))
         (i:set it2 'date (v:make-date (dt:datetime 2018 1 2))))
    (values st1 it1 it2 it3)))

(define date-tests
  (test-suite "Date"
    (test-suite "Copying with recurrence"
      (test-case "When date difference is less than skip, just one is created"
        (let-values (((st* items*) (copy-recur st1 it1 #:start (dt:datetime 2019 1 1)
                                                       #:end (dt:datetime 2019 1 2)
                                                       #:skip 30)))
          (check-equal? (length items*) 1)))

      (let-values (((st* items*)
                    (copy-recur st1 it1 #:start (dt:datetime 2019 1 5)
                                        #:end (dt:datetime 2019 1 12)
                                        #:skip 2)))
        (test-case "Created items are new"
          (check set-disjoint? items* (list it1 it2 it3)))

        (test-case "Correct number of items are created"
          (check-equal? (length items*) 4))

        (test-case "Items have properties of copied item"
          (check-equal? (i:get st* (list-ref items* 0) 'description) (v:make-string "Item 1"))
          (check-equal? (i:get st* (list-ref items* 1) 'description) (v:make-string "Item 1"))
          (check-equal? (i:get st* (list-ref items* 2) 'tags) (v:make-set (set (v:make-string "tag1")))))

        (test-case "Items have correct dates set"
          (check-equal? (i:get st* (list-ref items* 0) 'date) (v:make-date (dt:datetime 2019 1 5)))
          (check-equal? (i:get st* (list-ref items* 1) 'date) (v:make-date (dt:datetime 2019 1 7)))
          (check-equal? (i:get st* (list-ref items* 2) 'date) (v:make-date (dt:datetime 2019 1 9)))
          (check-equal? (i:get st* (list-ref items* 3) 'date) (v:make-date (dt:datetime 2019 1 11))))

        (test-case "Items have correct wait dates set"
          (check-equal? (i:get st* (list-ref items* 0) 'wait) (v:make-date (dt:datetime 2019 1 3)))
          (check-equal? (i:get st* (list-ref items* 1) 'wait) (v:make-date (dt:datetime 2019 1 5)))
          (check-equal? (i:get st* (list-ref items* 2) 'wait) (v:make-date (dt:datetime 2019 1 7)))
          (check-equal? (i:get st* (list-ref items* 3) 'wait) (v:make-date (dt:datetime 2019 1 9)))))

      (test-case "When original item has no wait date, the copies don't have any either"
        (let-values (((st* items*)
                      (copy-recur st1 it2 #:start (dt:datetime 2019 1 5)
                                          #:end (dt:datetime 2019 1 12)
                                          #:skip 2)))
          (check-equal? (i:get st* (list-ref items* 0) 'wait) #f)
          (check-equal? (i:get st* (list-ref items* 1) 'wait) #f)
          (check-equal? (i:get st* (list-ref items* 2) 'wait) #f)
          (check-equal? (i:get st* (list-ref items* 3) 'wait) #f))))))
