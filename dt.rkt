#!/usr/local/bin/racket
#lang racket

(require "lib/dt.rkt" "lib/virtualize.rkt")

;; main
(begin
  (let ([manager (make-todo-manager)]
        [result '()]
        [command ""])
    (if (null? (vector->list (current-command-line-arguments)))
      (begin
        (set! result (manager "-l"))
        (set! command "-l"))
      (begin
        (set! result (apply manager (vector->list (current-command-line-arguments))))
        (set! command (car (vector->list (current-command-line-arguments))))))
    (cond ((string=? command "-l") (print-all result))
          ((string=? command "-a") (print-added))
          ((string=? command "-d") (print-deleted))
          ((string=? command "-f") (print-finished result))
          ((string=? command "-r") (print-redo-done result))
          ((string=? command "-v") (print-version result))
          ((string=? command "--clear") (print-flush-done))
          (else (print-help)))))
