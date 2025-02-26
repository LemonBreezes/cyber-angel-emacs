;;; cae/eshell/autoload/help.el -*- lexical-binding: t; -*-

;;;; Documentation Utilities for Eshell

;; Load required functions lazily
(autoload #'tldr-get-file-path-from-command-name "tldr")
(autoload #'Man-completion-table "man")

;;;###autoload
(defun cae-eshell-help-run-help (cmd)
  "Show help for command or function CMD.
This function tries multiple documentation sources in the following order:
1. Eshell alias functions
2. External commands via tldr or man pages
3. Elisp functions via helpful

Returns t if help was successfully displayed, nil otherwise."
  (interactive
   (list (current-word)))
  (cond
   ;; Case 1: Check for eshell alias
   ((eshell-find-alias-function cmd)
    (helpful-callable (eshell-find-alias-function cmd))
    t)
   
   ;; Case 2: Check for external command
   ((or (and (string-match-p "^\\*." cmd) 
             (setq cmd (substring cmd 1)))
        (eshell-search-path cmd))
    (or 
     ;; Try tldr first
     (when (tldr-get-file-path-from-command-name cmd)
       (tldr cmd)
       t)
     ;; Fall back to man page if available
     (when (Man-completion-table cmd nil nil)
       (display-buffer (man cmd))
       t)))
   
   ;; Case 3: Check for Elisp function
   ((functionp (intern cmd)) 
    (helpful-callable (intern cmd)) 
    t)))

;;;###autoload
(defun cae-eshell-tldr-to-man ()
  "Navigate from a tldr page to the corresponding man page.
Does nothing if no man page exists for the current command."
  (interactive)
  (if-let* ((cmd (buffer-substring-no-properties
                  (point-min)
                  (next-single-property-change (point-min) 'face)))
            ((Man-completion-table cmd nil nil))
            (Man-notify-method 'pushy))
      (man cmd)
    (message "No manual for %s" cmd)))

;;;###autoload
(defun cae-eshell-man-to-tldr ()
  "Navigate from a man page to the corresponding tldr page.
Does nothing if no tldr page exists for the current command."
  (interactive)
  (if-let* ((cmd (cl-loop for s in (split-string Man-arguments)
                          maximize (length s) into max
                          finally return s))
            ((tldr-get-file-path-from-command-name cmd)))
      (progn
        (set-window-buffer (selected-window) (get-buffer-create "*tldr*"))
        (with-current-buffer "*tldr*" 
          (tldr cmd)))
    (message "No tldr page for %s" cmd)))
