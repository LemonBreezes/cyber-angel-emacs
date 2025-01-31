;;; vanilla-emacs-configs/inbalanced-parens-after-slurping.el -*- lexical-binding: t; -*-

(defvar cae-test-code
  (pp-to-string
   '(defun cae-multi-pull-repositories ()
      "Pull the shared repositories and handle conflicts asynchronously."
      (interactive)
      (let ((output-buffer (get-buffer-create " *cae-multi-pull-repositories*"))
            (all-pulls-succeeded t)
            (processes '()))
        (dolist (repo-dir cae-multi-repositories)
          (let ((default-directory repo-dir))
            (when (file-directory-p (concat repo-dir "/.git"))
              (if (file-exists-p (concat repo-dir "/.git/index.lock"))
                  (message "Git lockfile exists in %s, skipping pull" repo-dir)
                (let ((process
                       (start-process
                        "git-pull-process"
                        output-buffer
                        "git" "pull" "--recurse-submodules=on-demand")))
                  (push process processes)
                  (set-process-sentinel
                   process
                   (lambda (proc event)
                     (when (memq (process-status proc) '(exit signal))
                       (if (/= (process-exit-status proc) 0)
                           (progn
                             (message "Git pull failed in %s" repo-dir)
                             ;; Optionally display the output buffer
                             (display-buffer output-buffer)
                             (setq all-pulls-succeeded nil))
                         (progn
                           (message "Git pull succeeded in %s" repo-dir)
                           ;; Start git submodule update --init --recursive
                           (let ((submodule-process
                                  (start-process
                                   "git-submodule-update-process"
                                   output-buffer
                                   "git" "submodule" "update" "--init" "--recursive")))
                             (push submodule-process processes)
                             (set-process-sentinel
                              submodule-process
                              (lambda (subproc subevent)
                                (when (memq (process-status subproc) '(exit signal))
                                  (if (/= (process-exit-status subproc) 0)
                                      (progn
                                        (message "Git submodule update failed in %s" repo-dir)
                                        ;; Optionally display the output buffer
                                        (display-buffer output-buffer)
                                        (setq all-pulls-succeeded nil))
                                    (message "Git submodule update succeeded in %s" repo-dir))
                                  ;; Check for conflicts in submodule update
                                  (with-current-buffer output-buffer
                                    (save-excursion
                                      (goto-char (point-max))
                                      (if (re-search-backward "CONFLICT" nil t)
                                          (progn
                                            (message "Conflict detected during git submodule update in %s" repo-dir)
                                            (display-buffer output-buffer)
                                            (setq all-pulls-succeeded nil))
                                        (message "Submodules updated successfully in %s" repo-dir))))
                                  ;; When all processes have finished, run 'doom sync' if needed
                                  (when (and (null (delq subproc (cl-remove-if #'process-live-p processes)))
                                             all-pulls-succeeded)
                                    (cae-multi--run-doom-sync))))))))
                       ;; When all processes have finished, run 'doom sync' if needed
                       (when (and (null (delq proc (cl-remove-if #'process-live-p processes)))
                                  all-pulls-succeeded)
                         (cae-multi--run-doom-sync))))))))))))))

(with-temp-buffer
  (insert cae-test-code)
  (beginning-of-buffer)
  (search-forward "repo-dir\)\)")
  (end-of-line)
  (lispy-slurp 2)
  (check-parens))
