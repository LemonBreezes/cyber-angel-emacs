;;; private/org/autoload/org-jira.el -*- lexical-binding: t; -*-


;;;###autoload (autoload 'cae-org-jira-hydra/body "private/org/autoload/org-jira" nil t)
(defhydra cae-org-jira-hydra (:color blue :foreign-keys run)
  ("q" nil "Exit" :exit t)
  ("bg" 'org-jira-get-boards "get boards" :column "boards")
  ("iv" 'org-jira-get-issues-by-board "get issues by board" :column "issues")
  ("ib" 'org-jira-browse-issue "browse issue" :column "issues")
  ("ig" 'org-jira-get-issues "get issues" :column "issues")
  ("ij" 'org-jira-get-issues-from-custom-jql "get issues from custom JQL" :column "issues")
  ("ih" 'org-jira-get-issues-headonly "get issues, head only" :column "issues")
  ;;(define-key org-jira-map (kbd "C-c if") 'org-jira-get-issues-from-filter-headonly)
  ;;(define-key org-jira-map (kbd "C-c iF") 'org-jira-get-issues-from-filter)
  ("iu" 'org-jira-update-issue "update issue" :column "issues")
  ("iw" 'org-jira-progress-issue "progress issue workflow" :column "issues")
  ("in" 'org-jira-progress-issue-next "progress issue workflow (next)" :column "issues")
  ("ia" 'org-jira-assign-issue "assign issue" :column "issues")
  ;;(define-key org-jira-map (kbd "C-c isr") 'org-jira-set-issue-reporter)
  ("ir" 'org-jira-refresh-issue "refresh issue" :column "issues")
  ("iR" 'org-jira-refresh-issues-in-buffer "refresh issues in buffer" :column "issues")
  ("ic" 'org-jira-create-issue "create issue" :column "issues")
  ("ik" 'org-jira-copy-current-issue-key "copy current issue key" :column "issues")
  ("sc" 'org-jira-create-subtask "create subtask" :column "subtasks")
  ("sg" 'org-jira-get-subtasks "get subtasks" :column "subtasks")
  ("cc" 'org-jira-add-comment "add comment" :column "comments")
  ("cu" 'org-jira-update-comment "update comment" :column "comments")
  ("wu" 'org-jira-update-worklogs-from-org-clocks "update worklogs from org clocks" :column "worklogs")
  ("tj" 'org-jira-todo-to-jira "todo to Jira" :column "todo")
  ("if" 'org-jira-get-issues-by-fixversion "get issues by fix version" :column "issues")
  ("J" (cmd! (find-file org-jira-download-dir)) "Jump to the Jira folder" :column "Misc"))
