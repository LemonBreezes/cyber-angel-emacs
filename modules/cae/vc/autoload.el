;;; private/vc/autoload.el -*- lexical-binding: t; -*-

(eval-when-compile (require 'magit nil t))

;;;###autoload
(defun cae-magit-insert-diff-upstream (&optional branch)
  "From
 https://www.reddit.com/r/emacs/comments/112t0uo/comment/j9g2r5n/?utm_source=share&utm_medium=web2x&context=3.

Meant to be used like:
(magit-add-section-hook \'magit-status-sections-hook
                        \'cae-magit-insert-diff-upstream
                        \'magit-insert-stashes)"
  (magit-insert-section (diff-upstream nil t)
    (if-let ((upstream-branch (magit-get-upstream-branch (or branch (magit-get-current-branch)))))
        (progn
          (magit-insert-heading (format "Diff with %s:" (substring-no-properties upstream-branch)))
          (magit--insert-diff t
            "diff" "--no-prefix" (format "%s..." upstream-branch))))))

;;;###autoload
(defalias 'cae-magit-insert-diff-upstream-master
  (apply-partially #'cae-magit-insert-diff-upstream "master"))

;;;###autoload
(defun cae-magit-add-PR-fetch-ref (&optional remote-name)
  "If refs/pull is not defined on a GH repo, define it.

If REMOTE-NAME is not specified, it defaults to the `remote' set
for the \"main\" or \"master\" branch."
  (let* ((remote-name (or remote-name
                          (magit-get "branch" "main" "remote")
                          (magit-get "branch" "master" "remote")))
         (remote-url (magit-get "remote" remote-name "url"))
         (fetch-refs (and (stringp remote-url)
                          (string-match "github" remote-url)
                          (magit-get-all "remote" remote-name "fetch")))
         ;; https://oremacs.com/2015/03/11/git-tricks/
         (fetch-address (format "+refs/pull/*/head:refs/pull/%s/*" remote-name)))
    (when fetch-refs
      (unless (member fetch-address fetch-refs)
        (magit-git-string "config"
                          "--add"
                          (format "remote.%s.fetch" remote-name)
                          fetch-address)))))
