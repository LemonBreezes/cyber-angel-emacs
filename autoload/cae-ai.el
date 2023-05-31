;;; autoload/cae-ai.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-ai-org-ai-on-buffer ()
  (interactive)
  (save-mark-and-excursion
    (mark-whole-buffer)
    (call-interactively #'org-ai-on-region)))

;;;###autoload
(defun cae-ai-lazy-load-org-ai ()
  (interactive)
  (org-ai-global-mode +1)
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

;;;###autoload
(defun cae-ai-org-ai-kill-region-at-point ()
  (interactive)
  (call-interactively #'org-ai-kill-region-at-point)
  (forward-char -1)
  (let ((end (point))
        (s (buffer-substring-no-properties
            (point)
            (progn (skip-syntax-backward "\\s ") (point)))))
    (kill-append (concat s "\n") t)
    (delete-region (point) end)))

;;;###autoload
(defun cae-ai-toggle-chatgpt-shell ()
  (interactive)
  (if (get-buffer-window "*chatgpt*")
      (delete-window (get-buffer-window "*chatgpt*"))
    (let ((chatgpt-shell-display-function #'pop-to-buffer))
      (call-interactively #'chatgpt-shell))))
