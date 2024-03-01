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
