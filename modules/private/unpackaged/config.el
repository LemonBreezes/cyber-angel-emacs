;;; private/unpackaged/config.el -*- lexical-binding: t; -*-

;;; Org
(when (modulep! :lang org)
  (map! :map org-mode-map
        "<return>" #'cae-unpackaged-org-return-dwim)

  (add-hook 'org-mode-hook #'cae-unpackaged-org-fix-blank-lines-before-save))

;;; Magit
(when (modulep! :tools magit)
  (defalias 'magit-status-goto-initial-section-1 'magit-status-goto-initial-section)

  (map! [remap magit-status] #'cae-unpackaged-magit-save-buffer-show-status)

  ;; Restore the window configuration after exiting `magit-status', since we are
  ;; deleting other windows with `cae-unpackaged-magit-save-buffer-show-status'.
  (after! magit
    (setopt magit-bury-buffer-function #'magit-restore-window-configuration))
  (advice-add #'cae-unpackaged-magit-status :before-until
              #'cae-unpackaged-magit-status-disable-when-gac-enabled-a))
