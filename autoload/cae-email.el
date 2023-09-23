;;; autoload/cae-email.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-notmuch-quit ()
  (interactive)
  (if (bound-and-true-p +notmuch-workspace-name)
      (+workspace/delete +notmuch-workspace-name)
    (call-interactively #'notmuch-bury-or-kill-this-buffer)))

;;;###autoload
(defun cae-org-msg-goto-body (&optional end)
  "Go to either the beginning or the end of the body.
END can be the symbol top, bottom, or nil to toggle."
  (interactive)
  (let ((initial-pos (point)))
    (org-msg-goto-body)
    (when (or (eq end 'top)
              (and (or (eq initial-pos (point)) ; Already at bottom
                       (<= initial-pos ; Above message body
                           (save-excursion
                             (message-goto-body)
                             (point))))
                   (not (eq end 'bottom))))
      (message-goto-body)
      (search-forward (format org-msg-greeting-fmt
                              (concat " " (org-msg-get-to-name)))))))
