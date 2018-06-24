#lang racket

(require "./db.rkt")

(provide indent-print print-todo print-in-order
         print-all print-help print-added
         print-deleted print-finished print-redo-done
         print-version print-flush-done)

;; wrap a string with a color
(define (chalk s c)
  (cond ((null? c) (string-append "\033[0m" s "\033[0m"))
        ((eq? 'red c) (string-append "\033[1;31m" s "\033[0m"))
        ((eq? 'blue c) (string-append "\033[1;34m" s "\033[0m"))
        ((eq? 'cyan c) (string-append "\033[1;36m" s "\033[0m"))
        ((eq? 'green c) (string-append "\033[1;32m" s "\033[0m"))
        ((eq? 'yellow c) (string-append "\033[1;33m" s "\033[0m"))
        ((eq? 'light-gray c) (string-append "\033[1;30m" s "\033[0m"))
        ((eq? 'black c) (string-append "\033[0;30m" s "\033[0m"))))

;; this procedure takes a comparison procedure and a list containing keys
;; returns a new list sorted by id
;;   keys are '("id:status"...)
;;   for instance: '("2:0" "3:1" "0:0" "1:0")
;; this is using insertion sort
(define (sort-by-id p keys sorted)
  (define (insert l b)
    (if (null? l)
      (list b)
      (let ([id1 (string->number
                   (car
                     (string-split
                       (car l) ":")))]
            [id2 (string->number
                   (car
                     (string-split b ":")))])
        (if (p id1 id2)
          (cons (car l) (insert (cdr l) b))
          (cons b l)))))
  (if (null? keys)
    sorted
    (sort-by-id p (cdr keys) (insert sorted (car keys)))))


;; indent spaces and display given strings
(define (indent-print . params)
  (display "     ")
  (for-each display params))

;; print a todo
(define (print-todo id status text)
  (indent-print
    (chalk id 'light-gray)
    " [ "
    (if (string=? status "0")
      (chalk "X" 'red)
      (chalk "O" 'green))
    " ]"
    (chalk " - " 'light-gray)
    text
    "\n"))

;; print finished items at last
(define (print-in-order keys [finished-count  0])
  (when (not (null? keys))
    (let ([text (car (db-get (car keys)))]
          [id (car (string-split (car keys) ":"))]
          [status (cadr (string-split (car keys) ":"))])
      (cond ((string=? status "1")
             (when (< finished-count 3)
               (print-todo id status text))
             (when (= finished-count 3)
               (indent-print ".....\n")
               (newline))
             (print-in-order (cdr keys) (+ finished-count 1)))
            (else
              (print-in-order (cdr keys) finished-count)
              (print-todo id status text))))))

;; print all todos
(define (print-all keys)
  (newline)
  (if (null? keys)
    (indent-print "Empty list!\n")
    (print-in-order (sort-by-id > keys '())))
  (newline))

;; print help
(define (print-help)
  (newline)
  (indent-print "No such command.\n")
  (newline)
  (indent-print "Usage:\n")
  (indent-print "  => dt [options] [args...]\n")
  (newline)
  (indent-print "Options:\n")
  (indent-print "  -a        Add an item           [string]\n")
  (indent-print "  -d        Delete an item by id  [string]\n")
  (indent-print "  -f        Finish an item by id  [string]\n")
  (indent-print "  -r        Redo an item by id    [string]\n")
  (indent-print "  -v        Print current version\n")
  (indent-print "  --clear   Clear all items\n")
  (newline))

(define (print-added)
  (newline)
  (indent-print "Added!\n")
  (newline))

(define (print-deleted)
  (newline)
  (indent-print "Deleted!\n")
  (newline))

(define (print-finished id)
  (newline)
  (if id
      (indent-print "Finished: " (car (db-get (string-append id ":1"))) "\n")
      (indent-print "No such id\n"))
  (newline))

(define (print-redo-done id)
  (newline)
  (if id
      (indent-print "Redo: " (car (db-get (string-append id ":0"))) "\n")
      (indent-print "No such id\n"))
  (newline))

;; print version
(define (print-version VERSION)
  (newline)
  (indent-print "Version: " VERSION "\n")
  (newline))

(define (print-flush-done)
  (newline)
  (indent-print "Cleared!\n")
  (newline))
