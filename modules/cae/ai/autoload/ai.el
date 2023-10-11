;;; private/ai/autoload/ai.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-org-ai-on-buffer ()
  (interactive)
  (save-mark-and-excursion
    (mark-whole-buffer)
    (call-interactively #'org-ai-on-region)))

;;;###autoload
(defun cae-org-ai-on-region-or-buffer ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'org-ai-on-region)
    (call-interactively #'cae-org-ai-on-buffer)))

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
  (require 'chatgpt-shell)
  (if-let* ((buf (chatgpt-shell--primary-buffer))
            (win (get-buffer-window (chatgpt-shell--primary-buffer))))
      (delete-window (get-buffer-window (chatgpt-shell--primary-buffer)))
    (let ((chatgpt-shell-display-function #'pop-to-buffer))
      (call-interactively #'chatgpt-shell))))

;;;###autoload
(defun cae-ai-chatgpt-quit-or-delete-char (arg)
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (eobp) proc (= (point) (marker-position (process-mark proc))))
        (kill-buffer (current-buffer))
      (delete-char arg))))

;;;###autoload
(defun cae-send-to-chatgpt-if-comma-a (f &rest args)
  (require 'chatgpt-shell)
  (let ((input (comint-get-old-input-default))
        (chatgpt-shell-prompt-query-response-style 'other-buffer)
        (display-buffer-alist '(("^\\*chatgpt\\* "
                                 (+popup-buffer)
                                 (actions)
                                 (side . top)
                                 (size)
                                 (window-width . 40)
                                 (window-height . 0.16)
                                 (slot) (vslot)
                                 (window-parameters (ttl . 0)
                                                    (quit . t)
                                                    (select . t)
                                                    (modeline)
                                                    (autosave)
                                                    (transient . t)
                                                    (no-other-window . t))))))
    (if (string-prefix-p "," input)
        (chatgpt-shell-send-to-buffer (substring input 1))
      (apply f args))))
