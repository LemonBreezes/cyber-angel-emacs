;;; private/org/config.el -*- lexical-binding: t; -*-

(use-package! org-rich-yank
  :defer t :init
  (map! :map org-mode-map
        "C-M-y" #'cae-org-rich-yank))

(defvar +org-exit-src-code-hook nil
  "Hook run just before exiting a org source block buffer.")
(advice-add #'org-edit-src-exit :before #'cae-org-run-exit-src-code-hooks)
(add-hook! '+org-exit-src-code-hook
  (defun +org-exit-src-trim-eob-lines-h ()
    (unless (memq this-command '(org-return +org/return))
      (ws-butler-trim-eob-lines))))

(advice-add #'org-insert-heading :after #'cae-org-set-created-timestamp)
(add-hook! 'org-capture-mode-hook
  (defun org-capture--insert-timestamp ()
    (when (org-at-heading-p)
      (cae-org-set-created-timestamp))))

;; I use a split keyboard and want `DEL' to clear priorities.
(defun cae-return-del-as-spc-a (ret)
  (if (memq ret '(?\C-? ?\C-h)) ?\s ret))
(defadvice! cae-allow-del-to-clear-priority-a (oldfun &rest args)
  :around #'org-priority
  (advice-add #'read-char-exclusive :filter-return #'cae-return-del-as-spc-a)
  (unwind-protect (apply oldfun args)
    (advice-remove #'read-char-exclusive #'cae-return-del-as-spc-a)))

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
        "]" #'cae-org-insert-checkbox-or-bracket
        "C-c C-v" #'cae-org-babel-cheatsheet))

(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 500000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook 'org-mode-hook #'locally-defer-font-lock)

(after! org
  (map! :map org-mode-map
        :localleader
        "l f" #'cae-org-insert-file-link))

(after! ox
  (add-to-list 'org-export-filter-final-output-functions #'cae-org-export-remove-zero-width-space t))

(when (modulep! +pretty)
  (load! "+pretty"))
