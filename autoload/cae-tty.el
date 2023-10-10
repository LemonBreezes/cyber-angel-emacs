;;; autoload/cae-tty.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-tty-disable-unicode ()
  (interactive)
  (after! org-eldoc
    (setq org-eldoc-breadcrumb-separator " -> "))
  (after! embrace
    (setq embrace-help-separator " -> "))
  (after! replace
    (setq query-replace-from-to-separator " -> "))
  (after! helm-files
    (setq helm-rsync-percent-sign "%"))
  (after! which-key
    (setq which-key-separator " -> "
          which-key-dont-use-unicode t))
  (after! eros
    (setq eros-eval-result-prefix "=> "))
  (after! yasnippet
    (setq yas-trigger-symbol " =>"))
  (after! minions
    (setq minions-mode-line-lighter "="))
  (after! anzu
    (setq anzu-replace-to-string-separator " -> "))
  (after! org-tidy
    (setq org-tidy-properties-inline-symbol "."))
  (after! mu4e-bookmarks
    (setq mu4e-modeline-unread-items '("U:" "U:")
          mu4e-modeline-all-read '("R:" "R:")
          mu4e-modeline-all-clear '("C:" "C:")
          mu4e-modeline-new-items '("N:" "N:")))
  (after! dirvish-attributes
    (setq dirvish-attributes
          (delq 'subtree-state
                (delq 'all-the-icons dirvish-attributes))
          dirvish-path-separators '("~" "/" "/")
          dirvish-subtree-prefix " |"
          dirvish-subtree-line-prefix " |"))
  (dolist (fn '(nerd-icons-faicon
                nerd-icons-octicon))
    (advice-add fn :override #'ignore))
  (remove-hook 'org-mode-hook #'+org-pretty-mode))
