#lang racket/gui

(require "../lib/dt.rkt" "../lib/db.rkt" "../lib/virtualize.rkt")

(define manager (make-todo-manager))

;; root container
(define outer-frame
  (new frame%
       [label "Dt"]
       [width 320]
       [height 200]
       [border 12]
       [x 800]
       [y 200]))

(define input-box
  (new text-field%
       [parent outer-frame]
       [label "Todo"]))

(define (refresh-todo-items-container)
  (let ([children (send todo-items-container get-children)])
    (map (lambda (child)
           (send todo-items-container delete-child child))
         children)
    (get-todo-items)))

(define (add-btn-callback btn event)
  (let ([text (send input-box get-value)])
    (manager "-a" text)
    (refresh-todo-items-container)))

(define add-btn
  (new button%
       [parent outer-frame]
       [label "Add"]
       [callback add-btn-callback]))

(define todo-items-container
  (new vertical-panel%
       [parent outer-frame]
       [alignment '(left top)]
       [stretchable-height #f]))

(define (render-todo-item key)
  (let* ([content (car (db-get key))]
         [id (car (string-split key ":"))]
         [container (new horizontal-panel%
                         [parent todo-items-container]
                         [vert-margin 4]
                         [style '(border hscroll)])]
         [checkbox (new check-box%
                        [label content]
                        [parent container])]
         [del-button (new button%
                          [parent container]
                          [label "✖"]
                          [stretchable-width #f]
                          [callback (lambda (b e)
                                      (manager "-d" id)
                                      (refresh-todo-items-container))])])
    #t))

(define (get-todo-items)
  (let ([keys (sort-by-id > (manager "-l") '())])
    (map render-todo-item keys)))

(get-todo-items)
(send outer-frame show #t)
