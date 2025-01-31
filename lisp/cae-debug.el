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

;;; Finding where messages are coming from

;; I use this to find where a message is coming from. For example, I do this:
;; (cae-debug-message "the cat")

(defvar cae-debug-messages-to-backtrace nil)

(defadvice! cae-debug-message-a (format-string &rest args)
  :before #'message
  (when (and cae-debug-messages-to-backtrace format-string)
    (let ((message (apply #'format format-string args)))
      (when (cl-find message cae-debug-messages-to-backtrace :test #'string=)
        (setq cae-debug-messages-to-backtrace (delete message cae-debug-messages-to-backtrace))
        (backtrace)))))

(defun cae-debug-backtrace-message-source (message)
  (push message cae-debug-messages-to-backtrace))

;;; Debugging `doom-escape-hook'.

(defvar doom-escape-hook-debug-enabled nil
  "Non-nil if doom-escape-hook debugging is enabled.")

(defvar doom-escape-hook-original nil
  "A backup of the original doom-escape-hook functions.")

(defun enable-doom-escape-hook-debug ()
  "Enable debugging for doom-escape-hook.
This wraps each function in doom-escape-hook with a debugging
wrapper that prints messages before and after the function runs."
  (interactive)
  (unless doom-escape-hook-debug-enabled
    ;; Backup the original hook functions
    (setq doom-escape-hook-original doom-escape-hook)
    ;; Wrap each function with a debugging wrapper
    (setq doom-escape-hook
          (mapcar (lambda (fn)
                    (let ((fn-name (cond
                                    ((symbolp fn) (symbol-name fn))
                                    ((byte-code-function-p fn) "<compiled lambda>")
                                    (t "<lambda>"))))
                      (lambda ()
                        (message "doom-escape-hook: Calling %s" fn-name)
                        (let ((result (funcall fn)))
                          (message "doom-escape-hook: Finished %s" fn-name)
                          result))))
                  doom-escape-hook-original))
    (setq doom-escape-hook-debug-enabled t)
    (message "doom-escape-hook debugging enabled.")))

(defun disable-doom-escape-hook-debug ()
  "Disable debugging for doom-escape-hook.
This restores the original doom-escape-hook functions."
  (interactive)
  (when doom-escape-hook-debug-enabled
    ;; Restore the original hook functions
    (setq doom-escape-hook doom-escape-hook-original)
    (setq doom-escape-hook-debug-enabled nil)
    (message "doom-escape-hook debugging disabled.")))

;; See also autoload/cae-debug.el
