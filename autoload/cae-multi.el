;;; autoload/cae-multi.el -*- lexical-binding: t; -*-

(defvar cae-multi-sync-running nil
  "Non-nil if `cae-multi-sync-repositories' is currently running.")

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

(defun cae-multi--run-git-process (repo-dir step-name cmd-args conflict-check next-step finalize output-buffer verb-level set-failure)
  "Run a git command asynchronously for REPO-DIR.
STEP-NAME is a string (e.g. \"fetch\").
CMD-ARGS is a list of arguments passed to git.
CONFLICT-CHECK is an optional function that takes OUTPUT-BUFFER and returns non-nil if a conflict is detected.
NEXT-STEP is an optional function to invoke on success.
FINALIZE is a function to call when the process completes.
OUTPUT-BUFFER is where process output is collected.
VERB-LEVEL controls messaging.
SET-FAILURE is a function called to mark failure (e.g. set all-ops-succeeded to nil)."
  (let ((proc (apply #'start-process
                     (concat "git-" step-name "-" (file-name-nondirectory repo-dir))
                     output-buffer
                     "git" cmd-args)))
    (set-process-sentinel
     proc
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (if (/= (process-exit-status proc) 0)
             (let ((error-msg (format "Git %s failed in repository %s" step-name repo-dir)))
               (message "%s" error-msg)
               (with-current-buffer output-buffer
                 (goto-char (point-max))
                 (insert (format "\nError: %s\n" error-msg)))
               (display-buffer output-buffer)
               (funcall set-failure)
               (funcall finalize))
           (if (and conflict-check (funcall conflict-check output-buffer))
               (progn
                 (message "Conflict detected during %s in %s" step-name repo-dir)
                 (with-current-buffer output-buffer
                   (goto-char (point-max))
                   (insert (format "\nError: Conflict detected in repository %s\n" repo-dir)))
                 (display-buffer output-buffer)
                 (funcall set-failure)
                 (funcall finalize))
             (progn
               (when (>= verb-level 1)
                 (message "Git %s succeeded in %s" step-name repo-dir))
               (if next-step
                   (funcall next-step)
                 (funcall finalize))))))))
    proc))

(defun cae-multi--get-repo-commit (repo-dir)
  "Return the current commit hash of the git repository in REPO-DIR as a string."
  (with-temp-buffer
    (let ((default-directory repo-dir))
      (when (eq (call-process "git" nil t nil "rev-parse" "HEAD") 0)
        (string-trim (buffer-string))))))
;;
;;;###autoload
(defun cae-multi-sync-repositories (&optional verb-level)
  (interactive (list (if current-prefix-arg 2 1)))
  (unless verb-level (setq verb-level 0))
  (if cae-multi-sync-running
      (progn
        (when (equal verb-level 1)
          (message "Warning: cae-multi-sync-repositories is already running."))
        nil)  ; exit immediately
    (setq cae-multi-sync-running t)
    (let ((start-time (current-time))
          (output-buffer (get-buffer-create " *cae-multi-sync-repositories*"))
          (all-ops-succeeded t)
          (pending-private 0)  ; count of repos entirely in doom-private-dir
          (private-changed nil)  ; flag: did any private repo change?
          (initial-hashes (make-hash-table :test #'equal))
          (pending-repos 0)    ; count of repos with active sync chains
          doom-sync-proc      ; will hold doom sync process if started
          sync-finalized)     ; flag to ensure we finalize only once
      (with-current-buffer output-buffer (erase-buffer))
      (cl-labels
          ((repo-sync-finished (repo-dir)
                               "Called at the end of a repository's sync chain.
If the repo is in doom-private, call repo-finalize.
Then decrement the pending counter and, if zero, clear the running flag."
                               (when (string-prefix-p (file-truename doom-private-dir)
                                                      (file-truename repo-dir))
                                 (repo-finalize repo-dir))
                               (setq pending-repos (1- pending-repos))
                               (when (zerop pending-repos)
                                 (setq cae-multi-sync-running nil)
                                 (if doom-sync-proc
                                     nil
                                   (finalize-all))))
           (repo-finalize (repo-dir)
                          "Finalize a repo in doom-private-dir."
                          (let ((old-hash (gethash repo-dir initial-hashes))
                                (new-hash (cae-multi--get-repo-commit repo-dir)))
                            (unless (equal old-hash new-hash)
                              (setq private-changed t))
                            (setq pending-private (1- pending-private))
                            (when (zerop pending-private)
                              (if private-changed
                                  (setq doom-sync-proc (cae-multi--run-doom-sync verb-level start-time #'finalize-all))
                                (when (>= verb-level 1)
                                  (message "No changes detected in doom-private repositories; skipping doom sync"))))))
           (maybe-finalize (repo-dir)
                           "Call repo-finalize only for a repo that lies within doom-private-dir."
                           (when (string-prefix-p (file-truename doom-private-dir)
                                                  (file-truename repo-dir))
                             (repo-finalize repo-dir)))

           (finalize-all ()
                         (unless sync-finalized
                           (setq sync-finalized t)
                           (when (>= verb-level 1)
                             (if all-ops-succeeded
                                 (message "All sync operations finished successfully in %.2f seconds"
                                          (float-time (time-subtract (current-time) start-time)))
                               (message "Sync operations finished with errors in %.2f seconds"
                                        (float-time (time-subtract (current-time) start-time)))))))
           (start-push-step (repo-dir)
                            (cae-multi--run-git-process
                             repo-dir
                             "push"
                             '("push")
                             nil
                             nil
                             (lambda () (repo-sync-finished repo-dir))
                             output-buffer
                             verb-level
                             (lambda () (setq all-ops-succeeded nil))))
           (start-merge-step (repo-dir)
                             (cae-multi--run-git-process
                              repo-dir
                              "rebase"
                              '("rebase" "origin/master")
                              (lambda (buf)
                                (with-current-buffer buf
                                  (save-excursion
                                    (goto-char (point-min))
                                    (re-search-forward "CONFLICT" nil t))))
                              (lambda () (start-push-step repo-dir))
                              (lambda () (repo-sync-finished repo-dir))
                              output-buffer
                              verb-level
                              (lambda () (setq all-ops-succeeded nil))))
           (start-fetch-step (repo-dir)
                             (cae-multi--run-git-process
                              repo-dir
                              "fetch"
                              '("fetch" "origin")
                              nil
                              (lambda () (start-merge-step repo-dir))
                              (lambda () (repo-sync-finished repo-dir))
                              output-buffer
                              verb-level
                              (lambda () (setq all-ops-succeeded nil)))))
        (dolist (repo-dir cae-multi-repositories)
          (let ((default-directory repo-dir))
            (when (file-directory-p (expand-file-name ".git" repo-dir))
              (if (file-exists-p (expand-file-name ".git/index.lock" repo-dir))
                  (when (>= verb-level 1)
                    (message "Git lockfile exists in %s, skipping update" repo-dir))
                (setq pending-repos (1+ pending-repos))
                (when (string-prefix-p (file-truename doom-private-dir)
                                       (file-truename repo-dir))
                  (puthash repo-dir (cae-multi--get-repo-commit repo-dir) initial-hashes)
                  (setq pending-private (1+ pending-private)))
                (start-fetch-step repo-dir)))))
        (when (zerop pending-repos)
          (setq cae-multi-sync-running nil))
        nil))))

(defun cae-multi--run-doom-sync (verb-level &optional start-time finalize-callback)
  "Run 'doom sync' asynchronously and redirect output to the output buffer.
VERB-LEVEL controls how much output is emitted.
FINALIZE-CALLBACK, if non-nil, is a function called when the process completes."
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
         (unless (= (process-exit-status proc) 0)
           (message "'doom sync' failed with exit code %d" (process-exit-status proc)))
         (when finalize-callback
           (funcall finalize-callback)))))
    process))

;;;###autoload
(defun cae-multi-update-submodules (&optional verb-level)
  "For every repository in `cae-multi-repositories', update git submodules asynchronously.
VERB-LEVEL controls output:
  0: Silent (errors only)
  1: Normal messages (default for interactive execution)
  2: Verbose output

When called interactively, no prefix yields level 1 and a prefix yields level 2."
  (interactive (list (if current-prefix-arg 2 1)))
  (unless verb-level (setq verb-level 0))
  (let ((start-time (current-time))
        (output-buffer (get-buffer-create " *cae-multi-submodule-update*"))
        (all-ops-succeeded t)
        (pending-processes 0))
    (with-current-buffer output-buffer (erase-buffer))
    (cl-labels
        ((finalize ()
                   (setq pending-processes (1- pending-processes))
                   (when (zerop pending-processes)
                     (if all-ops-succeeded
                         (when (>= verb-level 1)
                           (message "Submodule update finished successfully in %.2f seconds"
                                    (float-time (time-subtract (current-time) start-time))))
                       (when (>= verb-level 1)
                         (message "One or more submodule updates failed. See %s for details (took %.2f seconds)"
                                  (buffer-name output-buffer)
                                  (float-time (time-subtract (current-time) start-time)))))))
         (update-submodule-for-repo (repo-dir)
                                    (let ((default-directory repo-dir))
                                      (when (file-directory-p (expand-file-name ".git" repo-dir))
                                        (unless (file-exists-p (expand-file-name ".git/index.lock" repo-dir))
                                          (setq pending-processes (1+ pending-processes))
                                          (cae-multi--run-git-process
                                           repo-dir
                                           "submodule-update"
                                           '("submodule" "update" "--init" "--recursive")
                                           (lambda (buf)
                                             (with-current-buffer buf
                                               (save-excursion
                                                 (goto-char (point-max))
                                                 (re-search-backward "\\bCONFLICT\\b" nil t))))
                                           nil  ; No next-step.
                                           #'finalize
                                           output-buffer
                                           verb-level
                                           (lambda () (setq all-ops-succeeded nil))))))))
      (dolist (repo-dir cae-multi-repositories)
        (update-submodule-for-repo repo-dir)))))
