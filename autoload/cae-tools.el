;;; autoload/cae-tools.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ping-status ()
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
                                 (run-with-timer 5 5 'ping-status)
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
  (cl-letf ((symbol-function #'detached-session-in-context)
            (lambda () session))
    (call-interactively #'detached-describe-session)))
