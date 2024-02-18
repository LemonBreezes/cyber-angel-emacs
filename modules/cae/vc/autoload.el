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
(defun cae-magit-status-setup-upstream-diff-section ()
  (magit-add-section-hook 'magit-status-sections-hook
                          (apply-partially #'cae-magit-insert-diff-upstream
                                           (cadr (magit--get-default-branch)))
                        'magit-insert-stashes
                        nil t))

;; As an example, add this to your .dir-locals.el:
;;(when (and (derived-mode-p 'magit-status-mode)
;;           (fboundp #'cae-magit-insert-diff-upstream-master))
;;  (magit-add-section-hook 'magit-status-sections-hook
;;                          #'cae-magit-insert-diff-upstream-master
;;                          'magit-insert-stashes
;;                          nil t))

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
         ;; TODO Make this apply to all remotes and work for GitLab as well.
         (fetch-address (format "+refs/pull/*/head:refs/remotes/%s/pr/*" remote-name)))
    (when fetch-refs
      (unless (member fetch-address fetch-refs)
        (magit-git-string "config"
                          "--add"
                          (format "remote.%s.fetch" remote-name)
                          fetch-address)))))

;;;###autoload
(defun consult-gh-fork-current-repo (&optional name remote remote-name)
  "Forks the repo in the current directory as NAME.
if REMOTE is non-nil adds a remote to the current repo, otherwise queries the user
if both REMOT and REMOTE-NAME are non-nil, REMOTE-NAME is used as the name of the remote"
  (interactive)
  (if (consult-gh--get-repo-from-directory)
      (let* ((name (or name (read-string "name for forked repo: " (car (last (split-string (consult-gh--get-repo-from-directory) "\/"))))))
             (forkrepo (concat (consult-gh--get-current-username) "/" name))
             (remote (or remote (yes-or-no-p "add a remote? ")))
             (remote-name (or (and remote remote-name)
                              (and remote (read-string "name of remote? " "origin"))
                              (and remote "origin"))))
        (consult-gh--command-to-string "repo" "fork" (if remote "--remote") (if (and remote-name (not (equal remote-name "origin"))) "--remote-name" "") (if  (and remote-name (not (equal remote-name "origin"))) remote-name ""))
        (message (format "current repo was forked to %s" (propertize forkrepo 'face 'font-lock-warning-face)))
        (run-hook-with-args 'consult-gh-repo-post-fork-hook forkrepo)
        (let ((inhibit-message t))
          forkrepo))
    (consult-gh-repo-fork)))


