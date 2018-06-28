#lang racket/gui

(require "../lib/dt.rkt" "../lib/db.rkt" "../lib/virtualize.rkt")

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
       [label #f]
       [callback (lambda (t e)
                   (define e-type (send e get-event-type))
                   (when (eq? e-type 'text-field-enter)
                     (let ([text (send t get-value)])
                       (manager "-a" text)
                       (refresh-todo-items-container)
                       (send input-box set-value ""))))]))

(define (refresh-todo-items-container)
  (let ([children (send todo-items-container get-children)])
    (map (lambda (child)
           (send todo-items-container delete-child child))
         children)
    (get-todo-items)))

(define todo-items-container
  (new vertical-panel%
       [parent outer-frame]
       [alignment '(left top)]
       [style '(auto-vscroll)]))

(define (render-todo-item key)
  (let* ([content (car (db-get key))]
         [id (car (string-split key ":"))]
         [status (cadr (string-split key ":"))]
         [container (new horizontal-panel%
                         [parent todo-items-container]
                         [vert-margin 4]
                         [stretchable-height #f]
                         [style '(border auto-hscroll)])]
         [checkbox (new check-box%
                        [label content]
                        [parent container]
                        [value (string=? status "1")]
                        [callback (lambda (c e)
                                    (define val (send c get-value))
                                    (if val
                                        (manager "-f" id)
                                        (manager "-r" id))
                                    (refresh-todo-items-container))])]
         [del-button (new button%
                          [parent container]
                          [label "✖"]
                          [stretchable-width #f]
                          [callback (lambda (b e)
                                      (manager "-d" id)
                                      (refresh-todo-items-container))])])
    #t))

(define (get-todo-items)
  (let ([donekeys (sort-by-id > (get-all-done) '())]
        [notdonekeys (sort-by-id > (get-all-not-done) '())])
    (map render-todo-item (append notdonekeys donekeys))))

(get-todo-items)
(send outer-frame show #t)
