;;; private/eshell/autoload/help.el -*- lexical-binding: t; -*-

(autoload #'tldr-get-file-path-from-command-name "tldr")
(autoload #'Man-completion-table "man")

;; Adapted from `esh-help-run-help'
;;;###autoload
(defun +eshell-help-run-help (cmd)
  "Show help for the pointed command or functions CMD."
  (interactive
   (list (current-word)))
  (cond
   ((eshell-find-alias-function cmd)
    (helpful-callable (eshell-find-alias-function cmd))
    t)
   ((or (and (string-match-p "^\\*." cmd) (cl-callf substring cmd 1))
        (eshell-search-path cmd))
    (or (and (tldr-get-file-path-from-command-name cmd)
             (prog1 t (tldr cmd)))
        (and (Man-completion-table cmd nil nil)
             (prog1 t (display-buffer (man cmd))))))
   ((functionp (intern cmd)) (helpful-callable (intern cmd)) t)))

;;;###autoload
(defun +eshell-tldr-to-man ()
  "Go the manpage for current tldr command."
  (interactive)
  (if-let ((cmd (buffer-substring-no-properties
                 (point-min)
                 (next-single-property-change (point-min) 'face)))
           ((Man-completion-table cmd nil nil))
           (Man-notify-method 'pushy))
      (man cmd)
    (message "No manual for %s" cmd)))

;;;###autoload
(defun +eshell-man-to-tldr () "Go the tldr page for the current man pgae." (interactive)
       (if-let ((cmd (cl-loop for s in (split-string Man-arguments)
                              maximize (length s) into max
                              finally return s))
                ((tldr-get-file-path-from-command-name cmd)))
           (progn
             (set-window-buffer (selected-window) (get-buffer-create "*tldr*"))
             (with-current-buffer "*tldr*" (tldr cmd)))
         (message "No tldr page for %s" cmd)))
