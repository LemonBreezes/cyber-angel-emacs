;;; private/helm/autoload/helm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +helm--which-key-inhibit-hook ()
  (setq which-key-inhibit nil)
  (remove-hook 'pre-command-hook
               #'+helm--which-key-inhibit-hook))

;;;###autoload
(defun +helm-lazy-load ()
  (interactive)
  (require 'helm)
  (setq unread-command-events (list ?\C-x ?c))
  (setq which-key-inhibit t)

  (add-hook 'pre-command-hook #'+helm--which-key-inhibit-hook)
  (run-with-idle-timer
   which-key-idle-delay nil
   (lambda ()
     (when which-key-inhibit
       (which-key-show-keymap
        'helm-command-map)))))
