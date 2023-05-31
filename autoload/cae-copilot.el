;;; autoload/cae-copilot.el -*- lexical-binding: t; -*-

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
