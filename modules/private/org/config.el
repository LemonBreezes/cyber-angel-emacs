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
  ;;(map! :map worf-mode-map
  ;;      "[" nil
  ;;      "]" nil
  ;;      "<backtab>" nil
  ;;      "<S-iso-lefttab>" nil)
  (advice-add #'worf-property
              :after
              (cae-defun cae-org-worf-property-a ()
                ;; Jump infront of the property drawer
                (let ((parent (org-element-property :parent (org-element-at-point))))
                  (when (eq 'property-drawer (car parent))
                    (goto-char (org-element-property :begin parent))))))
  (advice-add #'worf-up
              :around
              (cae-defun cae-org-worf-up-a (oldfun arg)
                (if (eq 'property-drawer (car (org-element-at-point)))
                    (org-up-element)
                  (funcall oldfun arg))))
  (advice-add #'worf-down
              :around
              (cae-defun cae-org-worf-down-a (oldfun arg)
                (when (eq 'property-drawer (car (org-element-at-point)))
                  (org-up-element))
                (funcall oldfun arg)))
  (advice-add #'worf-add :after #'cae-org-set-created-timestamp)
  (when (modulep! :editor multiple-cursors)
    (after! multiple-cursors-core
      (add-to-list 'mc/unsupported-minor-modes #'worf-mode))))
