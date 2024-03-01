;;; lisp/cae-debug.el -*- lexical-binding: t; -*-

;; I use this as a simpler print statement. For example, I do this:
;; (+log 'test)
;; (+log 'test "the cat" 1 2 3) => Values: test, the cat, 1, 2, 3

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

;;; Tracing functions

;; I use these to debug functions. For example, I do this:
;; (backtrace! #'my-function)
;; (unbacktrace! #'my-function)
;; (backtrace! #'my-function '(arg1 arg2))
;; (unbacktrace! #'my-function)
;; To get backtraces and figure out why a function is being called with certain
;; arguments.

(defun cae-debug-backtrace-a (args)
  `(lambda (&rest fn-args)
     (when (or (null ,args) (equal ,args fn-args))
       (backtrace))))

(defmacro backtrace! (function &rest args)
  `(advice-add ,function :before ,(cae-debug-backtrace-a args)))

(defmacro unbacktrace! (function)
  `(advice-remove ,function #'cae-debug-backtrace-a))


;;; Power debugging

;; Use this with `vertico--exhibit' for example to debug completion tables.
;; Copied from here:
;; https://gist.github.com/jdtsmith/1fbcacfe677d74bbe510aec80ac0050c.

(defun cae-debug-reraise-error (func &rest args)
  "Call function FUNC with ARGS and re-raise any error which occurs.
Useful for debugging post-command hooks and filter functions, which
normally have their errors suppressed."
  (condition-case err
      (apply func args)
    ((debug error) (signal (car err) (cdr err)))))

(defun cae-debug-toggle-debugging-function-on-hidden-errors (func)
  "Toggle hidden error debugging for function FUNC."
  (interactive "aFunction: ")
  (cond
   ((advice-member-p #'cae-debug-reraise-error func)
    (advice-remove func #'cae-debug-reraise-error)
    (message "Debug on hidden errors disabled for %s" func))
   (t
    (advice-add func :around #'cae-debug-reraise-error)
    (message "Debug on hidden errors enabled for %s" func))))
