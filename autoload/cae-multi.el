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
(defun cae-multi-sync-repositories (&optional verb-level)
  "For every repository in `cae-multi-repositories' do the following asynchronously:
  1. git fetch origin
  2. git merge origin/master
  3. git push

If any of these steps fails (for example if a merge conflict is detected)
an error is reported. VERB-LEVEL controls output:
  0: Silent (errors only)
  1: Normal messages (default for interactive execution)
  2: Verbose output

After all repositories have been processed, if everything succeeded,
`cae-multi--run-doom-sync' is invoked to complete the sync.

When called interactively, no prefix yields level 1 and a prefix yields level 2."
  (interactive (list (if current-prefix-arg 2 1)))
  (unless verb-level (setq verb-level 0))
  (let ((output-buffer (get-buffer-create " *cae-multi-sync-repositories*"))
        (all-ops-succeeded t)
        (pending-processes 0))
    (with-current-buffer output-buffer (erase-buffer))
    (cl-labels
        ;; finalize: called after any process (or chain) is completed.
        ((finalize ()
                   (setq pending-processes (1- pending-processes))
                   (when (zerop pending-processes)
                     (if all-ops-succeeded
                         (cae-multi--run-doom-sync verb-level)
                       (when (>= verb-level 1)
                         (message "One or more git operations failed. See %s for details"
                                  (buffer-name output-buffer))))))
         ;; start-git-step: run a git command asynchronously in REPO-DIR.
         ;; STEP-NAME is used only for logging.
         ;; CMD-ARGS is the list of arguments to pass to git.
         ;; NEXT-STEP—if non-nil—is a function to call if the process succeeds.
         (start-git-step (repo-dir step-name cmd-args next-step)
                         (let ((proc (apply #'start-process
                                            (concat "git-" step-name "-" (file-name-nondirectory repo-dir))
                                            output-buffer
                                            "git" cmd-args)))
                           (set-process-sentinel
                            proc
                            (lambda (proc event)
                              (when (memq (process-status proc) '(exit signal))
                                (if (/= (process-exit-status proc) 0)
                                    (let ((error-msg
                                           ;; In the merge step, search for “CONFLICT” to report that fact.
                                           (if (and (string= step-name "merge")
                                                    (with-current-buffer output-buffer
                                                      (save-excursion
                                                        (goto-char (point-min))
                                                        (re-search-forward "CONFLICT" nil t))))
                                               (format "Merge conflict detected in repository %s" repo-dir)
                                             (format "Git %s failed in repository %s" step-name repo-dir))))
                                      (message "%s" error-msg)
                                      (with-current-buffer output-buffer
                                        (goto-char (point-max))
                                        (insert (format "\nError: %s\n" error-msg)))
                                      (display-buffer output-buffer)
                                      (setq all-ops-succeeded nil)
                                      (finalize))
                                  (progn
                                    (when (>= verb-level 1)
                                      (message "Git %s succeeded in %s" step-name repo-dir))
                                    (if next-step
                                        (funcall next-step)
                                      (finalize)))))))
                           proc))
         ;; For each repository we chain the steps:
         (start-push-step (repo-dir)
                          (start-git-step repo-dir "push" (list "push")
                                          (lambda () (handle-submodule repo-dir))))
         (start-merge-step (repo-dir)
                           (start-git-step repo-dir "merge" (list "merge" "origin/master")
                                           (lambda () (start-push-step repo-dir))))
         (start-fetch-step (repo-dir)
                           (start-git-step repo-dir "fetch" (list "fetch" "origin")
                                           (lambda () (start-merge-step repo-dir))))
         (handle-submodule (repo-dir)
           "Start the submodule update process for REPO-DIR and set its sentinel."
           (let ((submodule-process
                  (start-process "git-submodule-update-process"
                                 output-buffer
                                 "git" "submodule" "update" "--init" "--recursive")))
             (set-process-sentinel
              submodule-process
              (lambda (subproc subevent)
                (when (memq (process-status subproc) '(exit signal))
                  (if (/= (process-exit-status subproc) 0)
                      (progn
                        (message "Git submodule update failed in %s" repo-dir)
                        (with-current-buffer output-buffer
                          (goto-char (point-max))
                          (insert (format "\nError: Git submodule update failed in repository %s\n"
                                          repo-dir)))
                        (display-buffer output-buffer)
                        (setq all-ops-succeeded nil))
                    (when (>= verb-level 1)
                      (message "Git submodule update succeeded in %s" repo-dir)))
                  (with-current-buffer output-buffer
                    (save-excursion
                      (goto-char (point-max))
                      (if (re-search-backward "\\bCONFLICT\\b" nil t)
                          (progn
                            (message "Conflict detected during git submodule update in %s" repo-dir)
                            (insert (format "\nError: Conflict detected in repository %s\n" repo-dir))
                            (display-buffer output-buffer)
                            (setq all-ops-succeeded nil))
                        (when (>= verb-level 2)
                          (message "Submodules updated successfully in %s" repo-dir)))))
                  (finalize))))))
      ;; For each directory listed in `cae-multi-repositories' (a global list of repo paths)
      (dolist (repo-dir cae-multi-repositories)
        (let ((default-directory repo-dir))
          (when (file-directory-p (expand-file-name ".git" repo-dir))
            (if (file-exists-p (expand-file-name ".git/index.lock" repo-dir))
                (when (>= verb-level 1)
                  (message "Git lockfile exists in %s, skipping update" repo-dir))
              (setq pending-processes (1+ pending-processes))
              (start-fetch-step repo-dir)))))
      nil)))

(defun cae-multi--run-doom-sync (verb-level)
  "Run 'doom sync' asynchronously and redirect output to the output buffer.
VERB-LEVEL controls how much output is emitted."
  (let* ((output-buffer (get-buffer-create " *cae-multi-sync-repositories*"))
         (process
          (start-process
           "doom-sync-process"
           output-buffer
           "doom" "sync")))
    (set-process-sentinel
     process
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (if (= (process-exit-status proc) 0)
             (when (>= verb-level 1)
               (message "'doom sync' finished successfully"))
           (progn
             (when (>= verb-level 1)
               (message "'doom sync' failed with exit code %d" (process-exit-status proc))
               (display-buffer output-buffer)))))))))
