#lang racket

(provide make-todo-manager)

(require "./db.rkt" "./virtualize.rkt")

;; define the todo manager maker
(define make-todo-manager
  (lambda ()

    (define VERSION "1.1.1")

    ;; generate id
    (define (gen-id)
      (define (get-max-id keys max)
        (if (null? keys)
          max
          (let ([id (string->number
                      (car (string-split (car keys) ":")))])
            (if (> id max)
              (get-max-id (cdr keys) id)
              (get-max-id (cdr keys) max)))))
      (number->string (+ (get-max-id (db-getallkeys) 100) 1)))

    ;; => (list-index (list 1 2 3) 1)
    ;; => 2
    ;; => (list-index (list 1 2 3) 3)
    ;; => ""
    (define (list-index l i)
      (if (> (+ i 1) (length l))
        ""
        (list-ref l i)))

    ;; set status
    (define (switch-status id status)
      (let ([t1 (car (db-get (string-append id ":0")))]
            [t2 (car (db-get (string-append id ":1")))])
        (if (and (null? t1) (null? t2))
            #f
            (begin
              (db-del (string-append id ":0"))
              (db-del (string-append id ":1"))
              (if (null? t1)
                (db-add (string-append id ":" status) t2)
                (db-add (string-append id ":" status) t1))
              #t))))

    ;; dispatch available apis

    ;; => (add-todo "go for a walk")
    ;; => Added!
    (define (add-todo text)
      (let ([id (gen-id)]
            [status "0"])
        (let ([k (string-append id ":" status)])
          (db-add k text))))

    ;; delete an item
    (define (delete-todo id)
      (db-del (string-append id ":0"))
      (db-del (string-append id ":1")))

    ;; finish a todo
    (define (finish-todo id)
      (let ([re (switch-status id "1")])
        (if re
            id
            #f)))

    ;; redo a todo
    (define (redo-todo id)
      (let ([re (switch-status id "0")])
        (if re
            id
            #f)))

    ;; flush all todos
    (define (flush-todos)
      (db-flush))

    ;; dispatch actions
    (define dispatch
      (lambda (symbol . rest)
        (cond ((string=? symbol "-l") (db-getallkeys))
              ((string=? symbol "-a") (apply add-todo rest))
              ((string=? symbol "-d") (apply delete-todo rest))
              ((string=? symbol "-f") (apply finish-todo rest))
              ((string=? symbol "-r") (apply redo-todo rest))
              ((string=? symbol "-v") VERSION)
              ((string=? symbol "--clear") (flush-todos))
              (else #f))))

    dispatch))
