;;; private/vc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-magit-insert-diff-upstream ()
  "From
 https://www.reddit.com/r/emacs/comments/112t0uo/comment/j9g2r5n/?utm_source=share&utm_medium=web2x&context=3."
  (magit-insert-section (diff-upstream nil t)
    (if-let ((upstream-branch (magit-get-upstream-branch (magit-get-current-branch))))
        (progn
          (magit-insert-heading (format "Diff with %s:" (substring-no-properties upstream-branch)))
          (magit--insert-diff
            "diff" "--no-prefix" (format "%s..." upstream-branch))))))
