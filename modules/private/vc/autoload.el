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
(defhydra cae-magit-forge-post-hydra (:color blue)
  ("C-c C-c" forge-post-submit "Submit")
  ("C-c C-e" forge-post-dispatch  "Dispatch")
  ("C-c C-k" forge-post-cancel "Cancel"))
