;;; lisp/cae-debug.el -*- lexical-binding: t; -*-

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

(defun +backtrace-a (&rest args)
  (+log args)
  (unless args
    (backtrace)))

(defmacro backtrace! (function)
  `(advice-add ',function :before #'+backtrace-a))

(defmacro unbacktrace! (function)
  `(advice-remove ',function #'+backtrace-a))
