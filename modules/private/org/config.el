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
        "]" #'cae-org-insert-checkbox-or-bracket))

(use-package! org-jira
  :defer t :init
  (map! :leader
        "J" #'cae-org-jira-hydra/body)
  :config
  (map! :map org-jira-entry-mode-map
        "C-c i" nil
        "C-c s" nil
        "C-c c" nil
        "C-c w" nil
        "C-c t" nil
        "C-c p" nil
        "C-c b" nil)
  (make-directory (expand-file-name "~/.org-jira") t))

(use-package! worf
  :defer t :init
  (add-hook 'org-mode-hook #'worf-mode)
  :config
  (define-key worf-mode-map (kbd "C-M-g") #'consult-org-heading)
  (advice-add #'worf-property
              :after
              (cae-defun cae-org-worf-property-a ()
                ;; Jump infront of the property drawer
                (let ((parent (org-element-property :parent (org-element-at-point))))
                  (when (eq 'property-drawer (car parent))
                    (goto-char (org-element-property :begin parent))))))
  (advice-add #'worf-add :after #'cae-org-set-created-timestamp)
  (when (modulep! :editor multiple-cursors)
    (after! multiple-cursors-core
      (add-to-list 'mc/unsupported-minor-modes #'worf-mode))))

;; From https://tecosaur.github.io/emacs-config/config.html
(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook 'org-mode-hook #'locally-defer-font-lock)

(defadvice! +org-indent--reduced-text-prefixes ()
  :after #'org-indent--compute-prefixes
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (when (> org-indent-indentation-per-level 0)
    (dotimes (n org-indent--deepest-level)
      (aset org-indent--text-line-prefixes
            n
            (org-add-props
                (concat (make-string (* n (1- org-indent-indentation-per-level))
                                     ?\s)
                        (if (> n 0)
                            (char-to-string org-indent-boundary-char)
                          "\u200b"))
                nil 'face 'org-indent)))))
