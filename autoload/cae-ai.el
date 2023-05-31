;;; autoload/cae-ai.el -*- lexical-binding: t; -*-

(defun cae-ai-org-ai-on-buffer ()
  (interactive)
  (save-mark-and-excursion
    (mark-whole-buffer)
    (call-interactively #'org-ai-on-region)))

;;;###autoload
(defun cae-ai-lazy-load-org-ai ()
  (interactive)
  (require 'org-ai)
  (setq unread-command-events
        (listify-key-sequence (kbd "C-c M-a")))
  (setq which-key-inhibit t)
  (add-transient-hook! 'pre-command-hook
    (setq which-key-inhibit nil))
  (run-with-idle-timer
   which-key-idle-delay nil
   (lambda ()
     (when which-key-inhibit
       (which-key-show-keymap 'org-ai-global-mode-prefix-map t)))))

;;org-ai-kill-region-at-point
(defun cae-ai-org-ai-kill-region-at-point ()
  (interactive)
  (let ((beg (org-ai--get-region-beginning))
        (end (org-ai--get-region-end)))
    (kill-region beg end)))
