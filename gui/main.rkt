#lang racket/gui

(require "../lib/dt.rkt")

(define manager (make-todo-manager))

;; root container
(define outer-frame
  (new frame%
       [label "Dt"]
       [width 320]
       [height 500]
       [border 12]
       [x 800]
       [y 200]))

(define input-box
  (new text-field%
       [parent outer-frame]
       [label "Todo"]))

(define add-btn
  (new button%
       [parent outer-frame]
       [label "Add"]
       [callback (lambda (btn event)
                   (let ([text (send input-box get-value)])
                     (manager "-a" text)))]))

(define todo-item
  (let* ([container
           (new vertical-panel%
                [parent outer-frame])]
         )
    #f))


(define (get-todo-items)
  (manager "-l"))
(pretty-print (get-todo-items))
;(send outer-frame show #t)
