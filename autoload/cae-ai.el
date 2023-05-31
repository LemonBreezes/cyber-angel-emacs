;;; autoload/cae-ai.el -*- lexical-binding: t; -*-

(defun cae-org-ai-on-buffer ()
  (interactive)
  (save-mark-and-excursion
    (mark-whole-buffer)
    (call-interactively #'org-ai-on-region)))

;;;###autoload
(cae-defun cae-lazy-load-org-ai ()
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
