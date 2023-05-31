;;; private/unpackaged/config.el -*- lexical-binding: t; -*-

;; This is a module created for stuff I copied from Alphapapa's `unpackaged.el'
;; package.

;;; Org
(when (modulep! :lang org)
  (map! :map org-mode-map
        "<return>" #'cae-unpackaged-org-return-dwim)

  (add-hook 'org-mode-hook #'cae-unpackaged-org-fix-blank-lines-before-save))

;;; Magit
(when (modulep! :tools magit)
  (defalias 'magit-status-goto-initial-section-1 'magit-status-goto-initial-section)

  (map! [remap magit-status] #'cae-unpackaged-magit-save-buffer-show-status
        [remap magit-status-here] #'cae-unpackaged-magit-save-buffer-show-status-here)

  ;; Restore the window configuration after exiting `magit-status', since we are
  ;; deleting other windows with `cae-unpackaged-magit-save-buffer-show-status'.
  (after! magit
    (setq magit-bury-buffer-function #'magit-restore-window-configuration)
    (map! :map magit-status-mode-map
          ;; Killing the Magit status buffer deletes the `forge-pull' progress
          ;; from the modeline.
          "q" #'magit-mode-bury-buffer))
  (advice-add #'cae-unpackaged-magit-status :before-until
              #'cae-unpackaged-magit-status-disable-when-gac-enabled-a)

  (defun cae-unpackaged-magit-start-smerge-hydra-h ()
    (when smerge-mode
      (+vc/smerge-hydra/body)))
  (add-hook 'magit-diff-visit-file-hook #'cae-unpackaged-magit-start-smerge-hydra-h))
