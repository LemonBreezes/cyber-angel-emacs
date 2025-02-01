;;; autoload/cae-debug.el -*- lexical-binding: t; -*-

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

;;;###autoload
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

;;; Debug garbage in dabbrev completion

;;;###autoload
(defun cae-search-in-dabbrev-buffers (search-string)
  "Search for SEARCH-STRING in all buffers returned by dabbrev--select-buffers."
  (interactive "sSearch string: ")
  (let ((buffers (dabbrev--select-buffers)))
    (multi-occur buffers search-string)))

;;; Debug transient

(require 'transient)
(require 'debug)

(defmacro cae-debug-define-toggle-command (name var)
  "Define a transient toggle command for debugging variable VAR with NAME."
  (let ((func-name (intern (concat "cae-debug-toggle-" name))))
    `(transient-define-suffix ,func-name ()
       ,(format "Toggle `%s'." var)
       :description (lambda ()
                      (format "Toggle %s [%s]" ',var (if ,var "ON" "OFF")))
       (interactive)
       (setq ,var (not ,var))
       (message "%s is now %s" ',var (if ,var "ON" "OFF"))
       (transient-setup 'cae-debug-transient))))

(transient-define-suffix cae-debug-set-debug-on-message ()
  "Set `debug-on-message' to a regular expression."
  :description (lambda ()
                 (format "Set debug-on-message [%s]" (if debug-on-message debug-on-message "None")))
  (interactive)
  (let ((regexp (read-regexp "Set debug-on-message to regexp: ")))
    (customize-set-variable 'debug-on-message
                            (if (string= regexp "") nil regexp)))
  (transient-setup 'cae-debug-transient))

(cae-debug-define-toggle-command "debug-on-error" debug-on-error)

(cae-debug-define-toggle-command "debug-on-quit" debug-on-quit)

(cae-debug-define-toggle-command "debug-on-signal" debug-on-signal)

(cae-debug-define-toggle-command "debug-allow-recursive-debug" debug-allow-recursive-debug)

(cae-debug-define-toggle-command "backtrace-on-redisplay-error" backtrace-on-redisplay-error)

(transient-define-suffix cae-debug-toggle-doom-debug-mode ()
  "Toggle `doom-debug-mode`."
  :description (lambda ()
                 (format "Toggle doom-debug-mode [%s]" (if doom-debug-mode "ON" "OFF")))
  (interactive)
  (doom-debug-mode 'toggle)
  (message "doom-debug-mode is now %s" (if doom-debug-mode "ON" "OFF"))
  (transient-setup 'cae-debug-transient))

;;;###autoload (autoload 'cae-debug-transient "autoload/cae-debug" nil t)
(transient-define-prefix cae-debug-transient ()
  "Set and toggle debugging options."
  ["Debugging Options"
   ("m" cae-debug-set-debug-on-message)
   ("e" cae-debug-toggle-debug-on-error)
   ("q" cae-debug-toggle-debug-on-quit)
   ("s" cae-debug-toggle-debug-on-signal)
   ("R" cae-debug-toggle-debug-allow-recursive-debug)
   ("r" cae-debug-toggle-backtrace-on-redisplay-error)
   ("d" cae-debug-toggle-doom-debug-mode)])
