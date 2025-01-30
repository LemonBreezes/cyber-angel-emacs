;;; autoload/cae-multi.el -*- lexical-binding: t; -*-

(defun cae-multi-commit-file (file)
  (when (file-in-directory-p file doom-user-dir)
    (let ((gac-automatically-push-p t)
          (gac-silent-message-p t))
      (gac--after-save (find-file-noselect file)))))

;;;###autoload
(defun cae-multi-bookmark-push-changes-a (&rest _)
  (gac--after-save bookmark-default-file))

;;;###autoload
(defun cae-multi-org-archive-push-changes-h ()
  (gac--after-save (buffer-file-name))
  (dolist (file (org-all-archive-files))
    (gac--after-save file)))

;;;###autoload
(defun cae-multi-abbrev-write-file-a (orig-fun &optional file verbose)
  (let ((mtime (nth 5 (file-attributes abbrev-file-name))))
    (if (or (null cae-multi-abbrev--file-mtime)
            (equal mtime cae-multi-abbrev--file-mtime))
        (progn (funcall orig-fun file verbose)
               (setq cae-multi-abbrev--file-mtime mtime)
               (unless file
                 (if cae-multi-abbrev--auto-commit-disabled
                     (when (y-or-n-p
                            (concat "Abbrev file modified since a previous "
                                    "save. Enable auto-commit?"))
                       (setq cae-multi-abbrev--auto-commit-disabled nil)
                       (cae-multi-commit-file abbrev-file-name))
                   (cae-multi-commit-file abbrev-file-name))))
      (message (concat "Abbrev file modified since last save. "
                       "Disabling abbrev file auto-commit."))
      (funcall orig-fun file verbose)
      (setq cae-multi-abbrev--file-mtime mtime
            cae-multi-abbrev--auto-commit-disabled t))))

;;;###autoload
(defun cae-multi-pull-repositories ()
  "Pull the shared repositories and handle conflicts."
  (interactive)
  (require 'org)
  (let ((doom-sync-needed nil))
    (dolist (repo-dir cae-multi-repositories)
      (let ((default-directory repo-dir))
        (when (file-directory-p (concat repo-dir "/.git"))
          (if (file-exists-p (concat repo-dir "/.git/index.lock"))
              (message "Git lockfile exists in %s, skipping pull" repo-dir)
            (with-temp-buffer
              (let ((exit-code (call-process "git" nil (current-buffer) nil "pull")))
                (if (/= exit-code 0)
                    (progn
                      (message "Git pull failed in %s with exit code %d" repo-dir exit-code)
                      (display-buffer (current-buffer)))
                  (goto-char (point-min))
                  (if (re-search-forward "CONFLICT" nil t)
                      (progn
                        (message "Conflict detected during git pull in %s" repo-dir)
                        (display-buffer (current-buffer)))
                    (message "Git pull succeeded in %s" repo-dir)))))))))))
