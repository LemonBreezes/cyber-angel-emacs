;;; cae/org/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

(use-package! org-rich-yank
  :defer t :init
  (map! :map org-mode-map
        "C-M-y" #'cae-org-rich-yank))

;; TODO Check if this is still necessary
(defvar +org-exit-src-code-hook nil
  "Hook run just before exiting a org source block buffer.")
(cae-advice-add #'org-edit-src-exit :before #'cae-org-run-exit-src-code-hooks)
(add-hook! '+org-exit-src-code-hook
  (defun +org-exit-src-trim-eob-lines-h ()
    (unless (memq this-command '(org-return +org/return))
      (ws-butler-trim-eob-lines))))

(cae-advice-add #'org-insert-heading :after #'cae-org-set-created-timestamp)
(add-hook! 'org-capture-mode-hook
  (defun org-capture--insert-timestamp ()
    (when (org-at-heading-p)
      (cae-org-set-created-timestamp))))

;; I use a split keyboard and want `DEL' to clear priorities.
(defun cae-return-del-as-spc-a (ret)
  (if (memq ret '(?\C-? ?\C-h)) ?\s ret))
(cae-defadvice! cae-allow-del-to-clear-priority-a (oldfun &rest args)
  :around #'org-priority
  (advice-add #'read-char-exclusive :filter-return #'cae-return-del-as-spc-a)
  (unwind-protect (apply oldfun args)
    (advice-remove #'read-char-exclusive #'cae-return-del-as-spc-a)))

(after! org
  (map! :map org-mode-map
        "]" #'cae-org-insert-checkbox-or-bracket
        "C-c C-v" #'cae-org-babel-cheatsheet))

(after! org
  (map! :map org-mode-map
        :localleader
        "l f" #'cae-org-insert-file-link))

(when (modulep! +pretty)
  (load! "+pretty"))
(load! "+tecosaur")
