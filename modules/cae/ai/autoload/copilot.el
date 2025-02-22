;;; autoload/cae-copilot.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-copilot-accept-completion-maybe ()
  "Accept the completion at point if there one or execute the
command currently typed."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion)
    (call-interactively
     (or (lookup-key (current-local-map)
                     (this-command-keys))
         (lookup-key (current-global-map)
                     (this-command-keys))))))

;;;###autoload
(defun cae-copilot-accept-completion-by-word-maybe ()
  "Accept the completion at point if there one or execute the
command currently typed."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion-by-word)
    (when-let* ((command (or (lookup-key (current-local-map)
                                         (this-command-keys))
                             (lookup-key (current-global-map)
                                         (this-command-keys)))))
      (call-interactively command))))

;;;###autoload
(defun cae-copilot-accept-completion-by-line-maybe ()
  "Accept the completion at point if there one or execute the
command currently typed."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion-by-line)
    (when-let* ((command (or (lookup-key (current-local-map)
                                         (this-command-keys))
                             (lookup-key (current-global-map)
                                         (this-command-keys)))))
      (call-interactively command))))

;;;###autoload
(defun cae-copilot-next-completion-maybe ()
  "Accept the completion at point if there one or execute the
command currently typed."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-next-completion)
    (when-let* ((command (or (lookup-key (current-local-map)
                                         (this-command-keys))
                             (lookup-key (current-global-map)
                                         (this-command-keys)))))
      (call-interactively command))))

;;;###autoload
(defun cae-copilot-previous-completion-maybe ()
  "Accept the completion at point if there one or execute the
command currently typed."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-previous-completion)
    (when-let* ((command (or (lookup-key (current-local-map)
                                         (this-command-keys))
                             (lookup-key (current-global-map)
                                         (this-command-keys)))))
      (call-interactively command))))

;;;###autoload
(defun cae-copilot-turn-on-safely ()
  (condition-case err
      (progn
        (copilot-mode +1)
        nil)
    (error
     (when (string-match-p "The package @github/copilot-language-server is not installed"
                           (error-message-string err))
       ;; Copilot is not going to be enabled after the server is installed but I
       ;; probably will not notice.
       (copilot-install-server)))))
