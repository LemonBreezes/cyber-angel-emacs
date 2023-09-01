;;; org/silly-loop.el -*- lexical-binding: t; -*-

(defun silly-loop (n)
  "Return the time, in seconds, to run N iterations of a loop."
  (let ((t1 (float-time)))
    (while (> (setq n (1- n)) 0))
    (- (float-time) t1)))

;; (silly-loop 50000000)
