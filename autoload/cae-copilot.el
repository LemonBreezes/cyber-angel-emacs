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
    (call-interactively
     (or (lookup-key (current-local-map)
                     (this-command-keys))
         (lookup-key (current-global-map)
                     (this-command-keys))))))

;;;###autoload
(defun cae-copilot-accept-completion-by-line-maybe ()
  "Accept the completion at point if there one or execute the
command currently typed."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion-by-line)
    (call-interactively
     (or (lookup-key (current-local-map)
                     (this-command-keys))
         (lookup-key (current-global-map)
                     (this-command-keys))))))

;;;###autoload
(defun cae-copilot-next-completion-maybe ()
  "Accept the completion at point if there one or execute the
command currently typed."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-next-completion)
    (call-interactively
     (or (lookup-key (current-local-map)
                     (this-command-keys))
         (lookup-key (current-global-map)
                     (this-command-keys))))))

;;;###autoload
(defun cae-copilot-previous-completion-maybe ()
  "Accept the completion at point if there one or execute the
command currently typed."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-previous-completion)
    (call-interactively
     (or (lookup-key (current-local-map)
                     (this-command-keys))
         (lookup-key (current-global-map)
                     (this-command-keys))))))
