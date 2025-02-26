;;; autoload/cae-multi.el -*- lexical-binding: t; -*-

(require 'cl-lib)

;;;; Variable Definitions

;; Sync status tracking
(defvar cae-multi-sync-running nil
  "Non-nil if `cae-multi-sync-repositories' is currently running.")
(defvar cae-multi-doom-dir-changes-detected nil
  "Non-nil if changes were detected in repositories inside doom-user-dir during sync.")
(defvar cae-multi-last-sync-duration 0
  "Time in seconds that the last sync operation took in `cae-multi-sync-repositories`.")
(defvar cae-multi-last-submodule-update-duration 0
  "Time in seconds that the last submodule update took.")

;; Abbrev tracking
(defvar cae-multi-abbrev-watch-descriptor nil
  "File notification descriptor for the abbrev file.")
(defvar cae-multi-abbrev--emacs-is-writing nil
  "Non-nil when Emacs is writing the abbrev file.")
(defvar cae-multi-abbrev--file-mtime nil
  "Last known modification time of the abbrev file.")
(defvar cae-multi--auto-save-abbrev-timer nil
  "Timer for deferred automatic saving of the abbrev file.")

;;;; Helper Functions

(defun cae-multi--is-valid-git-repo (repo-dir)
  "Check if REPO-DIR is a valid git repository that can be updated."
  (and (file-directory-p repo-dir)
       (file-directory-p (expand-file-name ".git" repo-dir))
       (not (file-exists-p (expand-file-name ".git/index.lock" repo-dir)))))

(defun cae-multi--detect-changes-in-git-output (output-buffer)
  "Detect if there are changes in the git output in OUTPUT-BUFFER.
Returns non-nil if changes are detected."
  (with-current-buffer output-buffer
    (save-excursion
      (goto-char (point-min))
      ;; Look for indicators of changes in git pull output
      (or (re-search-forward "\\(?:Updating\\|Fast-forward\\)" nil t)
          (re-search-forward "\\(?:[0-9]+ file\\|files?\\) changed" nil t)
          (re-search-forward "\\(?:create mode\\|delete mode\\)" nil t)
          (re-search-forward "\\(?:rename\\|copy\\)" nil t)
          (re-search-forward "^ [0-9]+ insertion" nil t)
          (re-search-forward "^ [0-9]+ deletion" nil t)))))

(defun cae-multi--handle-git-error (repo-dir step-name exit-status output-buffer verb-level set-failure finalize)
  "Handle git process errors."
  (let* ((output (with-current-buffer output-buffer (buffer-string)))
         (is-internet-error (or (string-match-p "ssh: Could not resolve hostname" output)
                               (string-match-p "fatal: Could not read from remote repository" output)))
         (error-msg (format "Git %s failed in repository %s (exit code %d)" 
                           step-name repo-dir exit-status)))
    (if is-internet-error
        (when (>= verb-level 1)
          (message "Network error during git %s in %s" step-name repo-dir)
          (with-current-buffer output-buffer
            (goto-char (point-max))
            (insert (format "\nNetwork error: %s\n" error-msg))))
      (with-current-buffer output-buffer
        (goto-char (point-max))
        (insert (format "\nError: %s\n" error-msg)))
      (unless (string= step-name "push")
        (message "%s" error-msg)
        (display-buffer output-buffer)))
    (funcall set-failure)
    (funcall finalize)))

(defun cae-multi--handle-git-conflict (repo-dir step-name output-buffer set-failure finalize)
  "Handle git conflicts."
  (message "Conflict detected during %s in %s" step-name repo-dir)
  (with-current-buffer output-buffer
    (goto-char (point-max))
    (insert (format "\nError: Conflict detected in repository %s\n" repo-dir)))
  (display-buffer output-buffer)
  (funcall set-failure)
  (funcall finalize))

(defun cae-multi--handle-git-success (repo-dir step-name output-buffer verb-level next-step finalize)
  "Handle successful git operations."
  ;; Check for changes in doom-user-dir repos during pull
  (when (and (string= step-name "pull")
             (string-prefix-p (file-truename doom-user-dir)
                              (file-truename repo-dir)))
    (when (cae-multi--detect-changes-in-git-output output-buffer)
      (setq cae-multi-doom-dir-changes-detected t)
      (when (>= verb-level 1)
        (message "Changes detected in %s, will run doom sync" repo-dir))))
  
  (when (>= verb-level 1)
    (message "Git %s succeeded in %s" step-name repo-dir))
  (if next-step
      (funcall next-step)
    (funcall finalize)))

(defun cae-multi--create-git-process-sentinel (repo-dir step-name conflict-check next-step
                                              finalize output-buffer verb-level set-failure)
  "Create a process sentinel for git operations."
  (lambda (proc event)
    (when (memq (process-status proc) '(exit signal))
      (let ((exit-status (process-exit-status proc)))
        (if (/= exit-status 0)
            (cae-multi--handle-git-error repo-dir step-name exit-status 
                                        output-buffer verb-level set-failure finalize)
          (if (and conflict-check (funcall conflict-check output-buffer))
              (cae-multi--handle-git-conflict repo-dir step-name output-buffer set-failure finalize)
            (cae-multi--handle-git-success repo-dir step-name output-buffer verb-level next-step finalize)))))))

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
     (cae-multi--create-git-process-sentinel 
      repo-dir step-name conflict-check next-step finalize output-buffer verb-level set-failure))
    proc))

(defun cae-multi--run-doom-sync (verb-level &optional start-time finalize-callback)
  "Run 'doom sync' asynchronously and redirect output to the output buffer.
VERB-LEVEL controls how much output is emitted.
START-TIME, if provided, is used for timing the operation.
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
         (let ((exit-status (process-exit-status proc)))
           (unless (= exit-status 0)
             (message "'doom sync' failed with exit code %d" exit-status)
             (with-current-buffer output-buffer
               (goto-char (point-max))
               (insert (format "\nError: 'doom sync' failed with exit code %d\n" exit-status))))
           (when finalize-callback
             (funcall finalize-callback))))))
    process))

;;;; Main Functions

;;;###autoload
(defun cae-multi-sync-repositories (&optional verb-level)
  "Sync all repositories in `cae-multi-repositories`.
VERB-LEVEL controls output verbosity (0=silent, 1=normal, 2=verbose)."
  (interactive (list (if current-prefix-arg 2 1)))
  (unless verb-level (setq verb-level 0))
  ;; Reset the changes detected flag at the start of sync
  (setq cae-multi-doom-dir-changes-detected nil)
  
  ;; Check if git is already running
  (if (cl-find-if (lambda (proc)
                    (and (process-live-p proc)
                         (let ((cmd (process-command proc)))
                           (and cmd (string= (car cmd) "git")))))
                  (process-list))
      (progn
        (when (>= verb-level 1)
          (message "Not running cae-multi-sync-repositories: Git subprocesses are active."))
        nil)
    
    ;; Check if sync is already running
    (if cae-multi-sync-running
        (progn
          (when (>= verb-level 1)
            (message "Warning: cae-multi-sync-repositories is already running."))
          nil)
      
      ;; Start the sync process
      (setq cae-multi-sync-running t)
      (let ((start-time (current-time))
            (output-buffer (get-buffer-create " *cae-multi-sync-repositories*"))
            (all-ops-succeeded t)
            (pending-private 0)    ; count of repos entirely in doom-user-dir
            (pending-repos 0)      ; count of repos with active sync chains
            doom-sync-proc         ; will hold doom sync process if started
            sync-finalized)        ; flag to ensure we finalize only once
        
        ;; Clear output buffer
        (with-current-buffer output-buffer (erase-buffer))
        
        ;; Define helper functions using cl-labels
        (cl-labels
            ((repo-sync-finished (repo-dir)
               "Called at the end of a repository's sync chain."
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
                 (if cae-multi-doom-dir-changes-detected
                     (setq doom-sync-proc 
                           (cae-multi--run-doom-sync verb-level start-time #'finalize-all))
                   (when (>= verb-level 1)
                     (message "No changes detected in doom-user-dir repos, skipping doom sync"))
                   (finalize-all))))
             
             (finalize-all ()
               "Finalize the entire sync operation."
               (unless sync-finalized
                 (setq sync-finalized t)
                 (let ((elapsed (float-time (time-subtract (current-time) start-time))))
                   (setq cae-multi-last-sync-duration elapsed)
                   (when (>= verb-level 1)
                     (if all-ops-succeeded
                         (message "All sync operations finished successfully in %.2f seconds" elapsed)
                       (message "Sync operations finished with errors in %.2f seconds" elapsed))))))
             
             (start-push-step (repo-dir)
               "Start the push step for a repository."
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
               "Start the fetch step for a repository."
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
          
          ;; Process each repository
          (dolist (repo-dir cae-multi-repositories)
            (let ((default-directory repo-dir)
                  (buffer-file-name repo-dir))
              (when (cae-multi--is-valid-git-repo repo-dir)
                (setq pending-repos (1+ pending-repos))
                (when (string-prefix-p (file-truename doom-user-dir)
                                       (file-truename repo-dir))
                  (setq pending-private (1+ pending-private)))
                (start-fetch-step repo-dir))))
          
          ;; Handle case where no repositories were processed
          (when (zerop pending-repos)
            (setq cae-multi-sync-running nil))
          nil)))))

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
                                    (when (cae-multi--is-valid-git-repo repo-dir)
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
                                       (lambda () (setq all-ops-succeeded nil))))))
      (dolist (repo-dir cae-multi-repositories)
        (update-submodule-for-repo repo-dir)))))

;;;; Abbrev File Handling

;;;###autoload
(defun cae-multi--update-abbrev-mtime ()
  "Update the stored modification time of the abbrev file."
  (when (and abbrev-file-name (file-exists-p abbrev-file-name))
    (setq cae-multi-abbrev--file-mtime
          (nth 5 (file-attributes abbrev-file-name)))))

;;;###autoload
(defun cae-multi--abbrev-changed-externally-p (new-mtime)
  "Check if abbrev file was changed externally based on NEW-MTIME."
  (and (not cae-multi-abbrev--emacs-is-writing)
       new-mtime  ;; Make sure file exists
       (or (null cae-multi-abbrev--file-mtime)
           (not (time-equal-p new-mtime cae-multi-abbrev--file-mtime)))))

;;;###autoload
(defun cae-multi-stop-abbrev-watch ()
  "Stop watching the abbrev file for external changes."
  (when cae-multi-abbrev-watch-descriptor
    (file-notify-rm-watch cae-multi-abbrev-watch-descriptor)
    (setq cae-multi-abbrev-watch-descriptor nil)
    (message "Stopped watching abbrev file.")))

;;;###autoload
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
Reload abbrevs only if the file was changed externally."
  (when (memq (cadr event) '(changed attribute-changed))
    (let* ((new-mtime (and (file-exists-p abbrev-file-name)
                           (nth 5 (file-attributes abbrev-file-name)))))
      (when (cae-multi--abbrev-changed-externally-p new-mtime)
        (condition-case err
            (progn
              (message "Abbrev file changed externally – reloading abbrevs…")
              (with-demoted-errors "Error reading abbrevs: %S"
                (read-abbrev-file abbrev-file-name t))
              (setq cae-multi-abbrev--file-mtime new-mtime)
              (message "Abbrevs reloaded."))
          (error
           (message "Error reloading abbrevs: %s" (error-message-string err))))))))

;;;###autoload
(defun cae-multi--schedule-auto-save-abbrev ()
  "Schedule an automatic save of the abbrev file during idle time."
  (unless cae-multi--auto-save-abbrev-timer
    (setq cae-multi--auto-save-abbrev-timer
          (run-with-idle-timer
           0.5 nil
           (lambda ()
             (when (and abbrevs-changed
                        (not cae-multi-abbrev--auto-commit-disabled))
               (condition-case err
                   (let ((cae-multi-abbrev--emacs-is-writing t))
                     (unwind-protect
                         (progn
                           (write-abbrev-file abbrev-file-name nil)
                           ;; Update our stored mtime after successful write
                           (cae-multi--update-abbrev-mtime)
                           (when (file-exists-p abbrev-file-name)
                             (cae-multi--push-changes abbrev-file-name " *cae-multi-abbrev-push-changes-a*")))
                       (setq cae-multi-abbrev--emacs-is-writing nil)))
                 (error
                  (setq cae-multi-abbrev--emacs-is-writing nil)
                  (message "Error saving abbrevs: %s" (error-message-string err)))))
             (setq cae-multi--auto-save-abbrev-timer nil))))))
