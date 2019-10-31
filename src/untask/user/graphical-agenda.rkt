#lang racket

(require untask/src/untask/core/serialize
         untask/src/untask/core/state
         (prefix-in v: untask/src/untask/core/value)
         (prefix-in p: untask/src/untask/core/property)
         (prefix-in bp: untask/src/untask/properties/builtin)
         (prefix-in ctx: untask/src/untask/core/context)
         (prefix-in cmd: untask/src/untask/command/command)
         (prefix-in a: untask/src/attribute)
         untask/src/squiggle)

;; Example data

(define example (a:update-path ((load-state "../../../../../msc/tasks/tasks.t") state.context-state)
                               (λ (cst) (ctx:toggle cst '((reset) (on "weekly"))))))

;; Cache data to improve rerendering performance

(define agenda-view (cmd:agenda example #:filter '()))

;; GUI setup

(require racket/gui)

(define frame (new frame% (label "Agenda") (width 1200) (height 800)))
(send frame show #t)
(define canvas (new canvas% (parent frame) (style '(no-autoclear vscroll hscroll)) (paint-callback (λ (_1 _2) (rerender!)))))
(define dc (send canvas get-dc))

;; Main

(define (truncate-text text)
  (if (> (string-length text) 20)
      (string-append (substring text 0 20) "...")
      text))

(define (translate-color color-name)
  (case color-name
    ((#f) (make-color #xC0 #xC0 #xC0))
    ((red) (make-color #xE0 #x90 #x90))
    ((green) (make-color #x90 #xE0 #x90))
    ((yellow) (make-color #xC8 #xB0 #x90))
    ((blue) (make-color #x80 #xA0 #xE0))
    ((magenta) (make-color #xB0 #x80 #xC0))
    ((cyan) (make-color #x70 #xB8 #xC0))))

(define task-description-text-height 18)
(define task-description-font (make-font #:size task-description-text-height))
(define task-box-width 300)
(define padding 8)
(define line-height (let-values (((_1 line-height _2 _3) (begin (send dc set-font task-description-font)
                                                                (send dc get-text-extent ""))))
                      line-height))

(define (task-box-height-for-effort effort)
  (* (+ padding line-height padding) (+ 1 effort)))

(define (render-task-box dc #:day day #:previous-tasks-efforts previous-tasks-efforts #:effort effort #:description description #:color color)
  (define x (+ padding (* (+ task-box-width padding) day)))
  (define y (foldl (λ (task-effort y)
                     (+ y padding (task-box-height-for-effort effort)))
                   padding
                   previous-tasks-efforts))
  (send dc set-pen (make-color #x00 #x00 #x00) 0 'transparent)
  (send dc set-brush (translate-color color) 'solid)
  (send dc draw-rounded-rectangle x y task-box-width (* (+ padding line-height padding) (+ 1 effort)) 4)
  (send dc set-text-foreground (make-color #x30 #x30 #x30))
  (send dc set-font task-description-font)
  (send dc draw-text description (+ padding x) (+ padding y)))

(define temporary-hardcoded-task-effort 2) ; TODO

(define (render-tasks dc item-state tasks #:day day)
  (foldl (λ (task previous-tasks-efforts)
           (define effort temporary-hardcoded-task-effort)
           (define description (truncate-text (v:unwrap-string (p:get item-state task (bp:ref 'description)))))
           (define color (let ((c (p:get item-state task (bp:ref 'color))))
                           (and c (string->symbol (v:unwrap-string c)))))
           (render-task-box dc
                            #:day day
                            #:effort effort
                            #:previous-tasks-efforts previous-tasks-efforts
                            #:description description
                            #:color color)
           (append previous-tasks-efforts (list effort)))
         (list)
         tasks))

(define (render-agenda dc item-state agenda-view)
  (foldl (λ (view day)
           (render-tasks dc item-state (cdr view) #:day day)
           (+ 1 day))
         0
         agenda-view))

(define (rerender!)
  (send dc set-background (make-color #xE0 #xE0 #xE0))
  (send dc clear)
  (render-agenda dc (a:get-path (example state.item-state)) agenda-view))

;; Initialize

(define total-width
  (+ padding (* (+ task-box-width padding) (length agenda-view))))

(define total-height
  (exact-ceiling
    (apply max (map (λ (view)
                      (foldl (λ (task height)
                               (+ height padding (task-box-height-for-effort temporary-hardcoded-task-effort)))
                             padding
                             (cdr view)))
                    agenda-view))))

(send canvas init-auto-scrollbars total-width total-height 0 0)

(rerender!)
