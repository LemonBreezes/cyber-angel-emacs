;;; autoload/cae-tools.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-ping-status ()
  (interactive)
  (let ((buffer (generate-new-buffer "*internet*")))
    (make-process
     :name "internet"
     :connection-type 'pipe
     :buffer buffer
     :command (list "ping" "-q" "-c" "1" "8.8.8.8")
     :sentinel `(lambda (p e)
                  (with-current-buffer ',buffer
                    (goto-char (point-min))
                    (if (or (search-forward "unreachable" nil t)
                            (search-forward "errors" nil t))
                        (progn (unless (string-match "ping-status" (format "%s" timer-list))
                                 (cae-run-with-timer 5 5 "cae-ping-status"
                                                     'cae-ping-status)
                                 (message "no internet")))
                      (when (string-match "ping-status" (format "%s" timer-list))
                        (cancel-function-timers 'ping-status))
                      (message "internet working"))
                    (kill-buffer))))))

;;;###autoload
(defun cae-detached-attach-dwim (session)
  (interactive
   (pcase (buffer-local-value 'major-mode (current-buffer))
     ('vterm-mode (eval (cadr (interactive-form #'detached-vterm-attach))))
     ('eshell-mode (eval (cadr (interactive-form #'detached-eshell-attach-session))))
     ('shell-mode (eval (cadr (interactive-form #'detached-shell-attach-session))))
     (t (eval (cadr (interactive-form #'detached-attach-session))))))
  (pcase (buffer-local-value 'major-mode (current-buffer))
    ('vterm-mode (detached-vterm-attach session))
    ('eshell-mode (detached-eshell-attach-session session))
    ('shell-mode (detached-shell-attach-session session))
    (t (detached-attach-session session))))

;;;###autoload
(defun cae-detached-describe-session (session)
  (interactive (list (progn (require 'detached)
                            (detached-session-in-context))))
  (when-let* ((buffer (get-buffer-create "*detached-session-info*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert
       (string-trim
        (detached--session-header session)))
      (goto-char (point-min))
      (local-set-key (kbd "q") #'delete-window)
      (when (featurep 'evil)
        (evil-local-set-key 'normal (kbd "q") #'delete-window)))
    (pop-to-buffer buffer)))

(defun cae-envrc-file-mode-setup ()
  "Set up the after-save hook to run `envrc-allow' automatically.
This function is added to `envrc-file-mode-hook' so that it runs only in
buffers visiting .envrc files."
  (add-hook 'after-save-hook 'envrc-allow nil t))

;; Attach our setup function to the envrc file mode.
(add-hook 'envrc-file-mode-hook 'cae-envrc-file-mode-setup)
