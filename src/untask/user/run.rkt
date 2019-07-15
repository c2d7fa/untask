#lang racket

(provide run!)

(require
 "../command/interpret.rkt"
 "render-list.rkt"

 "../core/state.rkt"
 (prefix-in a: "../../attribute.rkt")
 (prefix-in term: "../../terminal.rkt"))

(define (write-file! path content)
  (call-with-output-file path #:exists 'replace
    (λ (out)
      (display content out))))

;; Run an interpretation by writing and reading to actual files, asking the user
;; for confirmation and writing to standard output. Update the state by setting
;; the boxed state.
(define (run! interpretation state-box)
  (match interpretation
    (`(value ,x) x)
    (`(list-items ,state ,items ,continue)
     (begin (displayln (render-listing (a:get (state state.item-data)) items))
            (run! (continue) state-box)))
    (`(info-items ,state ,items ,continue)
     (begin (displayln (render-listing-info (a:get (state state.item-data)) items))
            (run! (continue) state-box)))
    (`(get-state ,continue)
     (run! (continue (unbox state-box)) state-box))
    (`(read-file ,path ,continue)
     (run! (continue (port->string (open-input-file path) #:close? #t)) state-box))
    (`(write-file ,path ,content ,continue)
     (begin (write-file! path content)
            (run! (continue) state-box)))
    (`(error ,message ,continue)
     (begin (displayln (term:render `((bold) (red) ("Error: " ,message))))
            (run! (continue) state-box)))
    (`(message ,message ,continue)
     (begin (displayln (term:render `((bold) (blue) (,message))))
            (run! (continue) state-box)))
    (`(confirm ,prompt ,continue)
     (let ((answer (begin
                     (display (format "~a " prompt))
                     (let ((input (read-line)))
                       (or (string-prefix? input "y")
                           (string-prefix? input "Y"))))))
       (run! (continue answer) state-box)))
    (`(set-state ,state ,continue)
     (begin (set-box! state-box state)
            (run! (continue) state-box)))))
