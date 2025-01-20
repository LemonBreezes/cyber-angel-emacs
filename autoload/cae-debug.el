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

(transient-define-suffix cae-debug-set-debug-on-message ()
  "Set `debug-on-message' to a regular expression."
  :description (lambda ()
                 (format "Set debug-on-message [%s]" (if debug-on-message debug-on-message "None")))
  (interactive)
  (let ((regexp (read-regexp "Set debug-on-message to regexp: ")))
    (customize-set-variable 'debug-on-message
                            (if (string= regexp "") nil regexp)))
  (transient-setup 'cae-debug-transient))

(transient-define-suffix cae-debug-toggle-debug-on-error ()
  "Toggle `debug-on-error'."
  :description (lambda ()
                 (format "Toggle debug-on-error [%s]" (if debug-on-error "ON" "OFF")))
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error is now %s" (if debug-on-error "ON" "OFF"))
  (transient-setup 'cae-debug-transient))

(transient-define-suffix cae-debug-toggle-debug-on-quit ()
  "Toggle `debug-on-quit'."
  :description (lambda ()
                 (format "Toggle debug-on-quit [%s]" (if debug-on-quit "ON" "OFF")))
  (interactive)
  (setq debug-on-quit (not debug-on-quit))
  (message "debug-on-quit is now %s" (if debug-on-quit "ON" "OFF"))
  (transient-setup 'cae-debug-transient))

(transient-define-suffix cae-debug-toggle-debug-on-signal ()
  "Toggle `debug-on-signal'."
  :description (lambda ()
                 (format "Toggle debug-on-signal [%s]" (if debug-on-signal "ON" "OFF")))
  (interactive)
  (setq debug-on-signal (not debug-on-signal))
  (message "debug-on-signal is now %s" (if debug-on-signal "ON" "OFF"))
  (transient-setup 'cae-debug-transient))

(transient-define-suffix cae-debug-toggle-debug-on-next-call ()
  "Toggle `debug-on-next-call'."
  :description (lambda ()
                 (format "Toggle debug-on-next-call [%s]" (if debug-on-next-call "ON" "OFF")))
  (interactive)
  (setq debug-on-next-call (not debug-on-next-call))
  (message "debug-on-next-call is now %s" (if debug-on-next-call "ON" "OFF"))
  (transient-setup 'cae-debug-transient))

(transient-define-suffix cae-debug-toggle-debug-allow-recursive-debug ()
  "Toggle `debug-allow-recursive-debug'."
  :description (lambda ()
                 (format "Toggle debug-allow-recursive-debug [%s]" (if debug-allow-recursive-debug "ON" "OFF")))
  (interactive)
  (setq debug-allow-recursive-debug (not debug-allow-recursive-debug))
  (message "debug-allow-recursive-debug is now %s" (if debug-allow-recursive-debug "ON" "OFF"))
  (transient-setup 'cae-debug-transient))

;;;###autoload (autoload 'cae-debug-transient "autoload/cae-debug" nil t)
(transient-define-prefix cae-debug-transient ()
  "Set and toggle debugging options."
  ["Debugging Options"
   ("m" cae-debug-set-debug-on-message)
   ("e" cae-debug-toggle-debug-on-error)
   ("q" cae-debug-toggle-debug-on-quit)
   ("s" cae-debug-toggle-debug-on-signal)
   ("n" cae-debug-toggle-debug-on-next-call)
   ("r" cae-debug-toggle-debug-allow-recursive-debug)
   ])
