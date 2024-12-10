;;; cae/ai/autoload/chatgpt-shell.el -*- lexical-binding: t; -*-

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

;;;###autoload
(defun cae-ai-open-chatgpt-workspace ()
  (interactive)
  (+workspace-switch "*chatgpt*" t)
  (chatgpt-shell))

;;;###autoload
(defun cae-ai-open-dall-e-workspace ()
  (interactive)
  (+workspace-switch "*dall-e*" t)
  (dall-e-shell))

;;;###autoload
(defun cae-ai-toggle-dall-e-shell ()
  (interactive)
  (require 'dall-e-shell)
  (if-let* ((buf (when (dall-e-shell--shell-buffers)
                   (car (dall-e-shell--shell-buffers))))
            (win (get-buffer-window buf)))
      (+popup/close win)
    (let ((dall-e-shell-display-function #'pop-to-buffer))
      (call-interactively #'dall-e-shell))))
