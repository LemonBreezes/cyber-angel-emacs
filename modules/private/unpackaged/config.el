;;; private/alphapapa-unpackages/config.el -*- lexical-binding: t; -*-

(use-package! unpackaged
  :defer t
  :init
;;; Org
  (when (modulep! :lang org)
    (autoload 'org-web-tools--get-first-url "org-web-tools")
    (autoload 'unpackaged/org-element-descendant-of "unpackaged")

    (map! :map org-mode-map
          "<return>" #'my-unpackaged/org-return-dwim)

    ;; I don't use this functionality.
    (remove-hook 'org-mode-hook #'unpackaged/org-mark-read-only)

    ;; For some reason `unpackaged/org-fix-blank-lines' freezes Emacs when native-compiled.
    (advice-add #'unpackaged/org-fix-blank-lines :override #'+unpackaged/org-fix-blank-lines-a)

    ;; For some reason Org capture freezes when I use this.
    (defun +org-fix-blank-lines-before-save ()
                (add-hook 'before-save-hook
                          (defun +org-fix-all-blank-lines ()
                            ;; wrong-type-argument stringp nil
                            (ignore-errors (unpackaged/org-fix-blank-lines '(4))))))
    (add-hook 'org-mode-hook #'+org-fix-blank-lines-before-save))

;;; Magit
  ;; Fixes a void function error.
  (when (modulep! :tools magit)
    (defalias 'magit-status-goto-initial-section-1 'magit-status-goto-initial-section)

    (map! [remap magit-status] #'unpackaged/magit-status)

    ;; Restore the window configuration after exiting `magit-status', since we are
    ;; deleting other windows with `unpackaged/magit-save-buffer-show-status'.
    (after! magit
      (setq! magit-bury-buffer-function #'magit-restore-window-configuration))

    ;; Do not jump to changes if they have been automatically commited.
    (defun +unpackaged/magit-status--disable-when-gac-enabled-a ()
                  (and (eq this-command 'unpackaged/magit-save-buffer-show-status)
                       (or (bound-and-true-p git-auto-commit-mode)
                           (not (doom-project-root))
                           (not (magit-git-repo-p (doom-project-root)))
                           (not (buffer-file-name))
                           (not (magit-file-tracked-p (buffer-file-name))))))
    (advice-add #'unpackaged/magit-status :before-until
                #'+unpackaged/magit-status--disable-when-gac-enabled-a)

    (add-hook 'magit-diff-visit-file #'+magit-start-smerge-hydra-maybe)))
