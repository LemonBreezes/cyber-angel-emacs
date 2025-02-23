;;; autoload/cae-multi.el -*- lexical-binding: t; -*-

(require 'cl-lib)

;;;###autoload
(defun cae-multi--push-changes (file buf-name)
  "Push changes for FILE using a temporary buffer BUF-NAME.
Sets the buffer's default directory and file-name, enables auto-push,
calls `gac--after-save' and then resets the buffer-local values."
  (let ((buf (get-buffer-create buf-name)))
    (setf (buffer-local-value 'default-directory buf)
          (file-name-directory file)
          (buffer-local-value 'buffer-file-name buf)
          file
          (buffer-local-value 'gac-automatically-push-p buf)
          t)
    (gac--after-save buf)
    (setf (buffer-local-value 'default-directory buf) nil
          (buffer-local-value 'buffer-file-name buf) nil)))

(defvar cae-multi-sync-running nil
  "Non-nil if `cae-multi-sync-repositories' is currently running.")

(defvar cae-multi-last-submodule-update-duration 0
  "Time in seconds that the last submodule update took.")

(defvar cae-multi-last-sync-duration 0
  "Time in seconds that the last sync operation took in `cae-multi-sync-repositories`.")

(defun cae-multi--run-git-process (repo-dir step-name cmd-args
                                            conflict-check next-step
                                            finalize output-buffer
                                            verb-level set-failure)
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
             (let* ((output (with-current-buffer output-buffer (buffer-string)))
                    (is-internet-error (and (string-match "ssh: Could not resolve hostname" output)
                                            (string-match "fatal: Could not read from remote repository" output)))
                    (error-msg (format "Git %s failed in repository %s" step-name repo-dir)))
               (if is-internet-error
                   (progn
                     (when (>= verb-level 1)
                       (message "%s" error-msg)
                       (with-current-buffer output-buffer
                         (goto-char (point-max))
                         (insert (format "\nError: %s\n" error-msg)))))
                 (progn
                   (with-current-buffer output-buffer
                     (goto-char (point-max))
                     (insert (format "\nError: %s\n" error-msg)))
                   (unless (string= step-name "push")
                     (message "%s" error-msg)
                     (display-buffer output-buffer))))
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

;;;###autoload
(defun cae-multi-sync-repositories (&optional verb-level)
  (interactive (list (if current-prefix-arg 2 1)))
  (unless verb-level (setq verb-level 0))
  (if (cl-find-if (lambda (proc)
                    (and (process-live-p proc)
                         (let ((cmd (process-command proc)))
                           (and cmd (string= (car cmd) "git")))))
                  (process-list))
      (progn
        (when (equal verb-level 1)
          (message "Not running cae-multi-sync-repositories: Git subprocesses are active."))
        nil)
    (if cae-multi-sync-running
        (progn
          (when (equal verb-level 1)
            (message "Warning: cae-multi-sync-repositories is already running."))
          nil)                          ; exit immediately
      (setq cae-multi-sync-running t)
      (let ((start-time (current-time))
            (output-buffer (get-buffer-create " *cae-multi-sync-repositories*"))
            (all-ops-succeeded t)
            (pending-private 0)    ; count of repos entirely in doom-user-dir
            (pending-repos 0)           ; count of repos with active sync chains
            doom-sync-proc              ; will hold doom sync process if started
            sync-finalized)             ; flag to ensure we finalize only once
        (with-current-buffer output-buffer (erase-buffer))
        (cl-labels
            ((repo-sync-finished (repo-dir)
                                 "Called at the end of a repository's sync chain.
If the repo is in doom-private, call repo-finalize.
Then decrement the pending counter and, if zero, clear the running flag."
                                 (when (string-prefix-p (file-truename doom-user-dir)
                                                        (file-truename repo-dir))
                                   (repo-finalize repo-dir))
                                 (setq pending-repos (1- pending-repos))
                                 (when (zerop pending-repos)
                                   (setq cae-multi-sync-running nil)
                                   (if doom-sync-proc
                                       nil
                                     (finalize-all))))
             (repo-finalize (repo-dir)
                            "Finalize a repo in doom-user-dir."
                            (setq pending-private (1- pending-private))
                            (when (zerop pending-private)
                              (setq doom-sync-proc (cae-multi--run-doom-sync verb-level start-time #'finalize-all))))
             (maybe-finalize (repo-dir)
                             "Call repo-finalize only for a repo that lies within doom-user-dir."
                             (when (string-prefix-p (file-truename doom-user-dir)
                                                    (file-truename repo-dir))
                               (repo-finalize repo-dir)))

             (finalize-all ()
                           (unless sync-finalized
                             (setq sync-finalized t)
                             (let ((elapsed (float-time (time-subtract (current-time) start-time))))
                               (setq cae-multi-last-sync-duration elapsed)
                               (when (>= verb-level 1)
                                 (if all-ops-succeeded
                                     (message "All sync operations finished successfully in %.2f seconds" elapsed)
                                   (message "Sync operations finished with errors in %.2f seconds" elapsed))))))
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
                               (lambda ()
                                 (when (>= verb-level 1)
                                   (message "Skipping push error for repository: %s" repo-dir)))))
             (start-fetch-step (repo-dir)
                               (cae-multi--run-git-process
                                repo-dir
                                "pull"
                                '("pull" "--rebase")
                                (lambda (buf)
                                  (with-current-buffer buf
                                    (save-excursion
                                      (goto-char (point-min))
                                      (re-search-forward "CONFLICT" nil t))))
                                (lambda () (start-push-step repo-dir))
                                (lambda () (repo-sync-finished repo-dir))
                                output-buffer
                                verb-level
                                (lambda () (setq all-ops-succeeded nil)))))
          (dolist (repo-dir cae-multi-repositories)
            (let ((default-directory repo-dir)
                  (buffer-file-name repo-dir))
              (when (file-directory-p (expand-file-name ".git" repo-dir))
                (if (file-exists-p (expand-file-name ".git/index.lock" repo-dir))
                    (when (>= verb-level 1)
                      (message "Git lockfile exists in %s, skipping update" repo-dir))
                  (setq pending-repos (1+ pending-repos))
                  (start-fetch-step repo-dir)))))
          (when (zerop pending-repos)
            (setq cae-multi-sync-running nil))
          nil)))))

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
                     (let ((elapsed (float-time (time-subtract (current-time) start-time))))
                       (setq cae-multi-last-submodule-update-duration elapsed)
                       (if all-ops-succeeded
                           (when (>= verb-level 1)
                             (message "Submodule update finished successfully in %.2f seconds" elapsed))
                         (when (>= verb-level 1)
                           (message "One or more submodule updates failed. See %s for details (took %.2f seconds)"
                                    (buffer-name output-buffer) elapsed))))))
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

;;; Hot reloading bookmarks

;;;###autoload
(defun cae-multi-bookmark-watch-callback (event)
  "Handle file change EVENT for the bookmark file.
If the file has been changed (or its attributes changed),
reload the bookmarks from `bookmark-default-file'."
  (when (memq (cadr event) '(changed attribute-changed))
    (message "Bookmark file changed—reloading bookmarks…")
    ;; The second argument t says to overwrite the current bookmarks;
    ;; the third argument t means no extra messages.
    (bookmark-load bookmark-default-file t t)
    (message "Bookmarks reloaded.")))

(defun cae-multi-stop-bookmark-watch ()
  "Stop watching the bookmark file for external changes."
  (when cae-multi-bookmark-watch-descriptor
    (file-notify-rm-watch cae-multi-bookmark-watch-descriptor)
    (setq cae-multi-bookmark-watch-descriptor nil)
    (message "Stopped watching the bookmark file.")))

;;; Hot reloading abbrevs

;;;###autoload
(defun cae-multi-stop-abbrev-watch ()
  "Stop watching the abbrev file for external changes."
  (when cae-multi-abbrev-watch-descriptor
    (file-notify-rm-watch cae-multi-abbrev-watch-descriptor)
    (setq cae-multi-abbrev-watch-descriptor nil)
    (message "Stopped watching abbrev file.")))

(defvar cae-multi-abbrev-watch-descriptor nil
  "File notification descriptor for the abbrev file.")

(defun cae-multi-start-abbrev-watch ()
  "Start watching the abbrev file for external changes.
When the abbrev file (given by the variable `abbrev-file-name`) changes,
the abbrevs are reloaded automatically."
  (when (and abbrev-file-name (file-exists-p abbrev-file-name))
    (unless cae-multi-abbrev-watch-descriptor
      (setq cae-multi-abbrev-watch-descriptor
            (file-notify-add-watch
             abbrev-file-name
             '(change)
             #'cae-multi-abbrev-watch-callback)))))

;;;###autoload
(defun cae-multi-abbrev-watch-callback (event)
  "Handle file change EVENT for the abbrev file.
Reload abbrevs only if the file was changed externally.
If the file's mtime is the same as our stored value, skip reloading."
  (when (memq (cadr event) '(changed attribute-changed))
    (let* ((new-mtime (nth 5 (file-attributes abbrev-file-name)))
           (old-mtime cae-multi-abbrev--file-mtime))
      (unless (and old-mtime (time-equal-p new-mtime old-mtime))
        (message "Abbrev file changed externally – reloading abbrevs…")
        (ignore-errors
          (read-abbrev-file abbrev-file-name t))
        (setq cae-multi-abbrev--file-mtime new-mtime)
        (message "Abbrevs reloaded.")))))
(defvar cae-multi--auto-save-abbrev-timer nil
  "Timer for deferred automatic saving of the abbrev file.")

;;;###autoload
(defun cae-multi--schedule-auto-save-abbrev ()
  "Schedule an automatic save of the abbrev file during idle time.
If a timer is already active, do not schedule another."
  (unless cae-multi--auto-save-abbrev-timer
    (setq cae-multi--auto-save-abbrev-timer
          (run-with-idle-timer
           0.5 nil
           (lambda ()
             (unless cae-multi-abbrev--auto-commit-disabled
               (when abbrevs-changed
                 (write-abbrev-file abbrev-file-name nil)
                 ;; Update our stored mtime.
                 (setq cae-multi-abbrev--file-mtime
                       (nth 5 (file-attributes abbrev-file-name)))
                 (cae-multi--push-changes abbrev-file-name " *cae-multi-abbrev-push-changes-a*")))
             (setq cae-multi--auto-save-abbrev-timer nil))))))
