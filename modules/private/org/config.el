;;; private/org/config.el -*- lexical-binding: t; -*-

(use-package! org-rich-yank
  :defer t
  :init
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

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(after! org
  (map! :map org-mode-map
        "]" #'+org-insert-checkbox-or-bracket))

(use-package! org-jira
  :defer t
  :config
  (make-directory (expand-file-name "~/.org-jira") t))
