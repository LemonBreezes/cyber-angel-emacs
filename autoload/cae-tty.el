;;; autoload/cae-tty.el -*- lexical-binding: t; -*-

(defun cae-terminal-type ()
  (cond
   ;; If Emacs is running in a GUI, you have full Unicode/font support.
   ((cae-display-graphic-p)
    2)
   ((getenv "WT_SESSION")
    1)
   ;; Linux virtual console (tty). The TERM variable is usually "linux"
   ((string= (or (getenv "TERM") "") "linux")
    0)
   ;; Otherwise, if LANG indicates UTF-8 you’re probably in a modern terminal emulator.
   ((and (getenv "LANG") (string-match "UTF-8" (getenv "LANG")))
    1)
   (t 0)))

;;;###autoload
(defun cae-tty-disable-unicode-and-or-icons ()
  (interactive)
  (when (= (cae-terminal-type) 0)
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
    (after! blamer
      (setq blamer-commit-formatter " * %s"))
    (after! dirvish
      (setq dirvish-attributes
            (delq 'subtree-state dirvish-attributes)
            dirvish-path-separators '("~" "/" "/")
            dirvish-subtree-prefix " |"
            dirvish-subtree-line-prefix " |"))
    (after! eglot
      (when (or (equal eglot-code-action-indicator (make-string 1 ?))
                (equal eglot-code-action-indicator (make-string 1 ?⚡)))
        (setq eglot-code-action-indicator (make-string 1 ?α))))
    (remove-hook 'org-mode-hook #'+org-pretty-mode))
  (when (< (cae-terminal-type) 2)
    (after! mu4e-bookmarks
      (setq mu4e-modeline-unread-items '("U:" "U:")
            mu4e-modeline-all-read '("R:" "R:")
            mu4e-modeline-all-clear '("C:" "C:")
            mu4e-modeline-new-items '("N:" "N:")))
    (after! marginalia
      (remove-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
    (after! corfu
      (setq corfu-margin-formatters
            (delq 'nerd-icons-corfu-formatter corfu-margin-formatters)))
    (after! dirvish
      (setq dirvish-attributes (delq 'nerd-icons
                                     (delq 'all-the-icons dirvish-attributes))))
    (dolist (fn '(nerd-icons-faicon
                  nerd-icons-octicon))
      (advice-add fn :override (cl-constantly "")))
    (remove-hook 'dired-mode-hook #'nerd-icons-dired-mode)))
