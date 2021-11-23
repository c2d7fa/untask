#lang racket

(provide display-graphical-agenda!)

(require racket/gui
         "../../untask/core/serialize.rkt"
         "../../untask/core/state.rkt"
         (prefix-in v: "../../untask/core/value.rkt")
         (prefix-in p: "../../untask/core/property.rkt")
         (prefix-in bp: "../../untask/properties/builtin.rkt")
         (prefix-in status: "../../untask/properties/status.rkt")
         (prefix-in ctx: "../../untask/core/context.rkt")
         (prefix-in cmd: "../../untask/command/command.rkt")
         (prefix-in a: "../../attribute.rkt")
         (prefix-in dt: "../../datetime.rkt")
         "../../squiggle.rkt")

(define (display-graphical-agenda! item-state agenda-view)
  (define frame (new frame% (label "Agenda") (width 1200) (height 800)))
  (define canvas (new canvas% (parent frame) (style '(no-autoclear vscroll hscroll)) (paint-callback (λ (_1 _2) (rerender! dc)))))
  (define dc (send canvas get-dc))

  (define (truncate-text text)
    (if (> (string-length text) 20)
        (string-append (substring text 0 20) "...")
        text))

  (define (translate-color color-name)
    (case color-name
      ((#f) (make-color #xA0 #xA0 #xA0))
      ((red) (make-color #xE0 #x90 #x90))
      ((green) (make-color #x90 #xE0 #x90))
      ((yellow) (make-color #xC8 #xB0 #x90))
      ((blue) (make-color #x80 #xA0 #xE0))
      ((magenta) (make-color #xB0 #x80 #xC0))
      ((cyan) (make-color #x70 #xB8 #xC0))
      ((done) (make-color #xD0 #xD0 #xD0))))

  (define task-description-text-height 18)
  (define task-description-font (make-font #:size task-description-text-height))
  (define heading-font (make-font #:size task-description-text-height #:weight 'bold))
  (define task-box-width 300)
  (define padding 8)
  (define line-height (let-values (((_1 line-height _2 _3) (begin (send dc set-font task-description-font)
                                                                  (send dc get-text-extent ""))))
                        line-height))
  (define header-padding (+ padding line-height padding))

  (define (task-box-height-for-effort effort)
    (+ (* (+ padding line-height padding) (+ 1 effort)) (* padding effort)))

  (define (render-task-box dc #:day day #:previous-tasks-efforts previous-tasks-efforts #:effort effort #:description description #:color color)
    (define x (+ padding (* (+ task-box-width padding) day)))
    (define y (foldl (λ (task-effort y)
                       (+ y padding (task-box-height-for-effort task-effort)))
                     (+ header-padding padding)
                     previous-tasks-efforts))
    (send dc set-pen (make-color #x00 #x00 #x00) 0 'transparent)
    (send dc set-brush (translate-color color) 'solid)
    (send dc draw-rounded-rectangle x y task-box-width (task-box-height-for-effort effort) 4)
    (send dc set-text-foreground (make-color #x30 #x30 #x30))
    (send dc set-font task-description-font)
    (send dc draw-text description (+ padding x) (+ padding y)))

  (define (render-heading dc #:day-index day #:date date)
    (define x (+ padding (* (+ task-box-width padding) day)))
    (define y padding)
    (send dc set-font heading-font)
    (send dc draw-text (dt:format-full-date-weekday date) x y))

  (define (render-day dc item-state tasks #:day-index day #:date date)
    (foldl (λ (task previous-tasks-efforts)
             (define effort (v:unwrap-number (p:get item-state task (bp:ref 'effort))))
             (define description (truncate-text (v:unwrap-string (p:get item-state task (bp:ref 'description)))))
             (define color (let ((c (p:get item-state task (bp:ref 'color))))
                             (if (status:done? item-state task)
                                 'done
                                 (and c (string->symbol (v:unwrap-string c))))))
             (render-task-box dc
                              #:day day
                              #:effort effort
                              #:previous-tasks-efforts previous-tasks-efforts
                              #:description description
                              #:color color)
             (render-heading dc #:day-index day #:date date)
             (append previous-tasks-efforts (list effort)))
           (list)
           tasks))

  (define (render-agenda dc item-state agenda-view)
    (foldl (λ (view day)
             (render-day dc item-state (cdr view) #:day-index day #:date (car view))
             (+ 1 day))
           0
           agenda-view))

  (define (rerender! dc)
    (send dc set-background (make-color #xE0 #xE0 #xE0))
    (send dc clear)
    (render-agenda dc item-state agenda-view))

  (define total-width
    (+ padding (* (+ task-box-width padding) (length agenda-view))))

  (define total-height
    (exact-ceiling
      (apply max (map (λ (view)
                        (foldl (λ (task height)
                                 (+ height padding (task-box-height-for-effort (v:unwrap-number (p:get item-state task (bp:ref 'effort))))))
                               (+ padding header-padding)
                               (cdr view)))
                      agenda-view))))

  (send canvas init-auto-scrollbars total-width total-height 0 0)
  (send frame show #t)

  (yield 'wait))
