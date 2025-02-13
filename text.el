;;; Text configuration

(when cae-init-text-enabled-p
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'auto-fill-mode)
  ;; ... (Text configuration continues) ...
  (after! org-journal
    (setq org-journal-file-format "%Y-%m-%d.org"))
  (when (modulep! :editor evil)
    (add-hook 'org-journal-after-entry-create-hook #'evil-insert-state))
  (after! markdown-mode
    (setq markdown-fontify-code-blocks-natively t)))
