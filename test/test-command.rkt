#lang racket

(require rackunit untask/test/util
         untask/src/untask/command/command

         (prefix-in dt: untask/src/datetime))

(provide command-tests)

;; Example state with dependencies and links between four items.
;;
;; 4 has child 1
;; 4 has child 2
;; 1 depends on 2
;; 2 depends on 3
;; 5 has child 1
;;
;; 1 has urgency 1; all others have urgency 0
(define st1 (load-example "example-2.t"))

;; Example state with dates.
;;
;; 1 has no date
;; 2, 3, 5 have date 2019-10-23
;; 4 has date 2019-10-27
;;
;; 2 has urgency 2
;; 5 has urgency 1
;; 3 has urgency 0
(define st2 (load-example "example-3.t"))

(define command-tests
  (test-suite "Commands"
    (test-suite "Tree"
      (test-case "Tree of non-matching filter expression gives no trees"
        (check-equal? (tree st1 #:filter '(not ()) #:post-filter '()) '()))

      (test-case "Tree of item with no children or dependencies gives just the item"
        (check-equal? (tree st1 #:filter '(item . 3) #:post-filter '()) '((3))))

      (test-case "Tree of item with one dependency"
        (check-equal? (tree st1 #:filter '(item . 2) #:post-filter '()) '((2 (3)))))

      (test-case "Tree of multiple items gives all trees"
        (check-equal? (tree st1 #:filter '(or (item . 3) (item . 2)) #:post-filter '()) '((3) (2 (3)))))

      (test-case "Tree of item with both multiple levels of nesting"
        (check-equal? (tree st1 #:filter '(item . 1) #:post-filter '()) '((1 (2 (3))))))

      (test-case "Hiding children at first level of nesting using #:post-filter"
        (check-equal? (tree st1 #:filter '(item . 1) #:post-filter '(not (item . 2))) '((1))))

      (test-case "Hiding children at second level of nesting using #:post-filter"
        (check-equal? (tree st1 #:filter '(item . 1) #:post-filter '(not (item . 3))) '((1 (2)))))

      (test-case "Tree of item with child link and multiple levels of nesting"
        (check-equal? (tree st1 #:filter '(item . 5) #:post-filter '()) '((5 (1 (2 (3)))))))

      (test-case "Tree with ellipses"
        (check-equal? (tree st1 #:filter '(item . 4) #:post-filter '()) '((4 (2 (3)) (1 (2 ()))))))

      (test-case "Multiple trees with ellipses caused by previous tree"
        (check-equal? (tree st1 #:filter '(or (item . 1) (item . 4)) #:post-filter '()) '((1 (2 (3))) (4 (2 ()) (1 (2 ()))))))

      (test-case "Using more advanced #:post-filter to filter trees"
        ;; TODO: We shouldn't require this order.
        (check-equal? (tree st1 #:filter '() #:post-filter '(not (status : (string . "inactive")))) '((1) (3) (2 (3)) (5) (4)))))

    (test-suite "Agenda"
      (test-case "Filter by date with single item"
        (check-equal? (agenda st2 #:filter '(date : (date 2019 10 27))) `((,(dt:datetime 2019 10 27) 4))))

      (test-case "Multiple items are sorted by urgency"
        (check-equal? (agenda st2 #:filter '(date : (date 2019 10 23))) `((,(dt:datetime 2019 10 23) 2 5 3))))

      (test-case "Showing multiple dates"
        (check-equal? (agenda st2 #:filter '(or (date : (date 2019 10 23))
                                                (date : (date 2019 10 27))))
                      `((,(dt:datetime 2019 10 23) 2 5 3)
                        (,(dt:datetime 2019 10 27) 4))))

      (test-case "Filter matching nothing gives empty agenda"
        (check-equal? (agenda st2 #:filter '(not ()))
                      '()))

      (test-case "Showing everything does not include items with no date"
        (check-equal? (agenda st2 #:filter '())
                      `((,(dt:datetime 2019 10 23) 2 5 3)
                        (,(dt:datetime 2019 10 27) 4)))))))
