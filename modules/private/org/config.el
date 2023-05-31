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
  :init
  (defvar org-jira-entry-mode-map
    (let ((org-jira-map (make-sparse-keymap)))
      (define-key org-jira-map (kbd "C-c pg") 'org-jira-get-projects)
      (define-key org-jira-map (kbd "C-c bg") 'org-jira-get-boards)
      (define-key org-jira-map (kbd "C-c iv") 'org-jira-get-issues-by-board)
      (define-key org-jira-map (kbd "C-c ib") 'org-jira-browse-issue)
      (define-key org-jira-map (kbd "C-c ig") 'org-jira-get-issues)
      (define-key org-jira-map (kbd "C-c ij") 'org-jira-get-issues-from-custom-jql)
      (define-key org-jira-map (kbd "C-c ih") 'org-jira-get-issues-headonly)
      ;;(define-key org-jira-map (kbd "C-c if") 'org-jira-get-issues-from-filter-headonly)
      ;;(define-key org-jira-map (kbd "C-c iF") 'org-jira-get-issues-from-filter)
      (define-key org-jira-map (kbd "C-c iu") 'org-jira-update-issue)
      (define-key org-jira-map (kbd "C-c iw") 'org-jira-progress-issue)
      (define-key org-jira-map (kbd "C-c in") 'org-jira-progress-issue-next)
      (define-key org-jira-map (kbd "C-c ia") 'org-jira-assign-issue)
      ;;(define-key org-jira-map (kbd "C-c isr") 'org-jira-set-issue-reporter)
      (define-key org-jira-map (kbd "C-c ir") 'org-jira-refresh-issue)
      (define-key org-jira-map (kbd "C-c iR") 'org-jira-refresh-issues-in-buffer)
      (define-key org-jira-map (kbd "C-c ic") 'org-jira-create-issue)
      (define-key org-jira-map (kbd "C-c ik") 'org-jira-copy-current-issue-key)
      (define-key org-jira-map (kbd "C-c sc") 'org-jira-create-subtask)
      (define-key org-jira-map (kbd "C-c sg") 'org-jira-get-subtasks)
      (define-key org-jira-map (kbd "C-c cc") 'org-jira-add-comment)
      (define-key org-jira-map (kbd "C-c cu") 'org-jira-update-comment)
      (define-key org-jira-map (kbd "C-c wu") 'org-jira-update-worklogs-from-org-clocks)
      (define-key org-jira-map (kbd "C-c tj") 'org-jira-todo-to-jira)
      (define-key org-jira-map (kbd "C-c if") 'org-jira-get-issues-by-fixversion)
      org-jira-map))
  :config
  (map! :map org-jira-entry-mode-map
        "C-c i" nil
        "C-c s" nil
        "C-c c" nil
        "C-c w" nil
        "C-c t" nil
        "C-c p" nil
        "C-c b" nil
        "<f6>" #'cae-org-jira-hydra/body)
  (make-directory (expand-file-name "~/.org-jira") t))
