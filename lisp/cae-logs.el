;;; lisp/cae-logs.el -*- lexical-binding: t; -*-

(defun +log-all (&rest values)
  `(message ,(concat "Values: "
                    (string-join (make-list (length values) "%s")
                                 ", "))
           ,@values))

(defvar-local +log-expander #'+log-all)

(defmacro +log (&rest values)
  (apply +log-expander values))

;; (macroexpand '(+log 'test))
;;      => (message "Values: %s" 'test)
