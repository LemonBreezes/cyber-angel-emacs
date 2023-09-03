;;; private/org/config.el -*- lexical-binding: t; -*-

(use-package! org-rich-yank
  :defer t :init
  (map! :map org-mode-map
        "C-M-y" #'cae-org-rich-yank))

(defvar +org-exit-src-code-hook nil
  "Hook run just before exiting a org source block buffer.")
(advice-add #'org-edit-src-exit :before #'cae-org-run-exit-src-code-hooks)
(add-hook '+org-exit-src-code-hook #'ws-butler-trim-eob-lines)

(advice-add #'org-insert-heading :after #'cae-org-set-created-timestamp)
(add-hook 'org-capture-mode-hook
  (cae-defun org-capture--insert-timestamp ()
    (when (org-at-heading-p)
      (cae-org-set-created-timestamp))))

;;(advice-add #'+org-init-keybinds-h :after
;;            (cae-defun cae-org-fixup-doom-keybindings ()
;;              (remove-hook 'org-tab-first-hook #'+org-indent-maybe-h)))

;; This is giving me an error.
(remove-hook 'org-mode-hook #'+org-make-last-point-visible-h)

(use-package! org-appear
  :defer t :init
  (add-hook 'org-mode-hook #'org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(after! org
  (map! :map org-mode-map
        "]" #'cae-org-insert-checkbox-or-bracket))

(use-package! org-modern
  :unless (cae-tty-disable-unicode-p)
  :defer t :after org
  :config
  (global-org-modern-mode +1)
  (setq org-modern-todo nil))

(use-package! org-tidy
  :defer t :init
  (add-hook 'org-mode-hook #'org-tidy-mode)
  :config
  (setq org-tidy-properties-inline-symbol (if (cae-tty-disable-unicode-p) "." "·")))

(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook 'org-mode-hook #'locally-defer-font-lock)
