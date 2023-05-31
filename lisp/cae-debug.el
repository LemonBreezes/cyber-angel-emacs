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

(defun cae-debug-backtrace-a (&rest args)
  (+log args)
  (unless args
    (backtrace)))

(defmacro backtrace! (function)
  `(advice-add ',function :before #'cae-debug-backtrace-a))

(defmacro unbacktrace! (function)
  `(advice-remove ',function #'cae-debug-backtrace-a))

;;; Power debugging

(defun cae-debug-reraise-error (func &rest args)
  "Call function FUNC with ARGS and re-raise any error which occurs.
Useful for debugging post-command hooks and filter functions, which
normally have their errors suppressed."
  (condition-case err
      (apply func args)
    ((debug error) (signal (car err) (cdr err)))))

(defun toggle-debug-on-hidden-errors (func)
  "Toggle hidden error debugging for function FUNC."
  (interactive "aFunction: ")
  (cond
   ((advice-member-p #'cae-debug-reraise-error func)
    (advice-remove func #'cae-debug-reraise-error)
    (message "Debug on hidden errors disabled for %s" func))
   (t
    (advice-add func :around #'cae-debug-reraise-error)
    (message "Debug on hidden errors enabled for %s" func))))

;; Use this with `vertico--exhibit' for example to debug completion tables.
