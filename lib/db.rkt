#lang racket

(provide db-add db-del db-get db-getallkeys db-flush)

(require redis kw-utils/partial )

(define conn (connect))
(define (db-execute command . args)
  (let ([re (apply (partial send-cmd #:rconn conn command) args)])
    (cond ((list? re) (map (lambda (item)
                             (if (bytes? item)
                                 (bytes->string/utf-8 item)
                                 item)) re))
          ((bytes? re) (list (bytes->string/utf-8 re)))
          ((char? re) (if (char=? re #\nul)
                          (list '())
                          (list (string re))))
          (else (list re)))))

(define (db-add k v)
  (db-execute "SET" k v))
(define (db-del k)
  (db-execute "DEL" k))
(define (db-getallkeys)
  (db-execute "KEYS" "*"))
(define (db-get k)
  (db-execute "GET" k))
(define (db-flush)
  (db-execute "FLUSHALL"))
