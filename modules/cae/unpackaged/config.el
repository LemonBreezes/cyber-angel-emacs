;;; cae/unpackaged/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

;; This is a module created for stuff I copied from Alphapapa's `unpackaged.el'
;; package.

;;; Org
(when (modulep! :lang org)
  (after! org
    (map! (:map org-mode-map
           :ig "<return>" #'cae-unpackaged-org-return-dwim
           :ig "RET" #'cae-unpackaged-org-return-dwim)
          (:when (modulep! :editor evil)
           (:after evil-org
            :map evil-org-mode-map
            :ig "<return>" nil
            :ig "RET" nil))))

  (add-hook 'org-mode-hook #'cae-unpackaged-org-fix-blank-lines-before-save))

;;; Magit
(when (modulep! :tools magit)
  (defalias 'magit-status-goto-initial-section-1 'magit-status-goto-initial-section)

  (map! [remap magit-status] #'cae-unpackaged-magit-save-buffer-show-status
        [remap magit-status-here] #'cae-unpackaged-magit-save-buffer-show-status-here)

  ;; Restore the window configuration after exiting `magit-status', since we are
  ;; deleting other windows with `cae-unpackaged-magit-save-buffer-show-status'.
  (after! magit
    (setq magit-bury-buffer-function #'magit-restore-window-configuration))
  (cae-advice-add #'cae-unpackaged-magit-status :before-until
              #'cae-unpackaged-magit-status-disable-when-gac-enabled-a)

  (defun cae-unpackaged-magit-start-smerge-hydra-h ()
    (when smerge-mode
      (+vc/smerge-hydra/body)))
  (add-hook 'magit-diff-visit-file-hook #'cae-unpackaged-magit-start-smerge-hydra-h))
