;;; private/vc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-magit-insert-diff-upstream (&optional branch)
  "From
 https://www.reddit.com/r/emacs/comments/112t0uo/comment/j9g2r5n/?utm_source=share&utm_medium=web2x&context=3.

Meant to be used like:
(magit-add-section-hook 'magit-status-sections-hook
                        'cae-magit-insert-diff-upstream
                        'magit-insert-stashes)"
  (magit-insert-section (diff-upstream nil t)
    (if-let ((upstream-branch (magit-get-upstream-branch (or branch (magit-get-current-branch)))))
        (progn
          (magit-insert-heading (format "Diff with %s:" (substring-no-properties upstream-branch)))
          (magit--insert-diff t
            "diff" "--no-prefix" (format "%s..." upstream-branch))))))

;;;###autoload
(defalias 'cae-magit-insert-diff-upstream-master
  (apply-partially #'cae-magit-insert-diff-upstream "master"))

;;;###autoload (autoload 'cae-magit-forge-post-hydra/body "private/vc/autoload" nil t)
(defhydra cae-magit-forge-post-hydra (:color pink)
  ("<f6>" nil "Exit" :exit t)
  ("C-c C-c" forge-post-submit "Submit")
  ("C-c C-e" forge-post-dispatch  "Dispatch")
  ("C-c C-k" forge-post-cancel "Cancel"))

;;;###autoload (autoload 'cae-magit-forge-topic-hydra/body "private/vc/autoload" nil t)
(defhydra cae-magit-forge-topic-hydra (:color pink)
  ("<f6>" nil "Exit" :exit t)
  ("C-c C-n" forge-create-post "Create post")
  ("C-c C-e" magit-edit-thing  "Edit")
  ("C-c C-w" magit-browse-thing "Open in browser"))

;;;###autoload (autoload 'cae-magit-forge-pullreq-list-hydra/body "private/vc/autoload" nil t)
(defhydra cae-magit-forge-pullreq-list-hydra (:color pink)
  ("<f6>" nil "Exit" :exit t)
  ("j" +default/search-buffer "Search buffer")
  ("o" forge-browse-topic "Open in browser"))

(defun cae-git-timemachine-help ()
  (interactive)
  (if transient--original-buffer
      (transient--stack-zap)
    (call-interactively  #'git-timemachine-help)))
