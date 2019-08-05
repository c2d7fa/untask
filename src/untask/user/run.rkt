#lang racket

(provide run!)

(require
 "../command/interpret.rkt"
 "render-list.rkt"

 "../core/state.rkt"
 (prefix-in a: "../../attribute.rkt")
 (prefix-in term: "../../terminal.rkt")

 (only-in "../../misc.rkt" try-read-line))

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
    (`(agenda-items ,state ,blocks ,continue)
     (begin (displayln (render-agenda (a:get (state state.item-data)) blocks))
            (run! (continue) state-box)))
    (`(get-state ,continue)
     (run! (continue (unbox state-box)) state-box))
    (`(read-file ,path ,continue)
     (if (or (eq? #f path)
             (not (file-exists? path)))
         (run! (continue #f) state-box)
         (run! (continue (port->string (open-input-file path) #:close? #t)) state-box)))
    (`(write-file ,path ,content ,continue)
     (begin (write-file! path content)
            (run! (continue) state-box)))
    (`(error ,message ,continue)
     (begin (displayln (term:render `((bold) (red) ("Error: " ,message))))
            (run! (continue) state-box)))
    (`(message ,message ,continue)
     (begin (displayln (term:render `((blue) (,message))))
            (run! (continue) state-box)))
    (`(confirm ,prompt ,continue)
     (let ((answer (begin
                     (display (term:render `((bold) (blue) (,(format "~a " prompt)))))
                     (let ((input (try-read-line)))
                       (or (not input)
                           (string-prefix? input "y")
                           (string-prefix? input "Y"))))))
       (run! (continue answer) state-box)))
    (`(set-state ,state ,continue)
     (begin (set-box! state-box state)
            (run! (continue) state-box)))))
