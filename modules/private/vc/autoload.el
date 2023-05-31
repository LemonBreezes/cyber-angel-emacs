;;; private/vc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-magit-insert-diff-upstream (&optional branch)
  (magit-insert-section (diff-upstream nil t)
    (if-let ((upstream-branch (or branch (magit-get-upstream-branch (magit-get-current-branch)))))
        (progn
          (magit-insert-heading (format "Diff with %s:" (substring-no-properties upstream-branch)))
          (magit--insert-diff
            "diff" "--no-prefix" (format "%s..." upstream-branch))))))
