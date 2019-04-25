#lang racket

(require "user.rkt" "state.rkt")

;;

(define !state (box empty-state))

;; testing setup:
(execute !state '(add "eat food"))
(execute !state '(tag 0 "health"))
(execute !state '(add "buy milk"))
(execute !state '(tag 1 "shopping"))
(execute !state '(tag 1 "outside"))
(execute !state '(add "drink milk"))
(execute !state '(tag 2 "health"))
(execute !state '(add "go for a walk"))
(execute !state '(tag 3 "outside"))
(execute !state '(tag 3 "health"))
(execute !state '(done 1))
(execute !state '(pause 0))
(execute !state '(urg 0 +1.50))
(execute !state '(urg 1 -0.75))
(displayln "  all:")
(execute !state '(list-all))
(displayln "  search:")
(execute !state '(search (or (tag "outside") (not (and active (tag "health"))))))
