;;; autoload/cae-compile.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-compile-rebuild-package ()
  (when (and (stringp (buffer-file-name))
             (stringp (file-name-directory (buffer-file-name)))
             (string-prefix-p (expand-file-name ".local/straight/" doom-emacs-dir)
                              (file-name-directory (buffer-file-name))))
    (require 'straight)
    (when-let* ((package (straight--determine-repo buffer-file-name)))
      (mapc #'delete-file (directory-files (file-name-directory
                                            (buffer-file-name))
                                           nil
                                           "flycheck_.*"))
      (straight-rebuild-package package))))

;;;###autoload
(defun cae-ensure-emacs-dir-writable ()
  "Check if Emacs directories are writable and attempt to make them writable if not."
  (dolist (emacs-dir '("/usr/share/emacs" "/etc/emacs"))
    (when (and (file-exists-p emacs-dir)
               (not (file-writable-p emacs-dir)))
      (let ((sudo-cmd (format "sudo chmod -R u+w %s" emacs-dir)))
        (unless (zerop (shell-command sudo-cmd))
          (message "Warning: %s is not writable. Byte compilation may fail."
                   emacs-dir)
          (message "Failed to make %s writable. You may need to run: %s"
                   emacs-dir sudo-cmd))))))

;;;###autoload
(defun cae-compile-load-all-deferred-packages ()
  "Load all deferred packages."
  (interactive)
  (dolist (package (cl-nunion doom-incremental-packages
                              (flatten-list doom--deferred-packages-alist)))
    (require package nil t)))

;;;###autoload
(defun cae-compile-load-all-packages ()
  "Load all packages by finding .el files in the load path and requiring them.
This attempts to load every Elisp file found in the load path directories.
Results are displayed in a dedicated log buffer."
  (interactive)
  (let ((loaded-count 0)
        (error-count 0)
        (load-path-copy load-path)
        (already-tried (make-hash-table :test 'equal))
        (log-buffer (get-buffer-create "*Package Loading Log*")))
    ;; Set up the log buffer
    (with-current-buffer log-buffer
      (erase-buffer)
      (special-mode) ; Make it read-only with convenient navigation
      (insert "Package Loading Log\n")
      (insert "==================\n\n")
      (insert "Loading packages from load-path...\n\n"))
    
    (dolist (dir load-path-copy)
      (when (and dir (file-exists-p dir) (file-directory-p dir))
        (with-current-buffer log-buffer
          (insert (format "Scanning directory: %s\n" dir)))
        (dolist (file (directory-files dir t "\\.el$"))
          (let* ((filename (file-name-nondirectory file))
                 ;; Skip backup files, autoload files, etc.
                 (skip-file (or (string-match-p "^\\." filename)
                                (string-match-p "-autoloads\\.el$" filename)
                                (string-match-p "-pkg\\.el$" filename)
                                (string-match-p "^flycheck_" filename)))
                 (feature (unless skip-file
                            (intern (file-name-sans-extension filename)))))
            (when (and feature
                       (not (gethash feature already-tried))
                       (not (featurep feature)))
              (puthash feature t already-tried)
              (condition-case err
                  (progn
                    (require feature nil t)
                    (if (featurep feature)
                        (progn
                          (cl-incf loaded-count)
                          (with-current-buffer log-buffer
                            (insert (format "✓ Loaded: %s\n" feature))))
                      (with-current-buffer log-buffer
                        (insert (format "⚠ Not loaded: %s\n" feature)))))
                (error
                 (cl-incf error-count)
                 (with-current-buffer log-buffer
                   (insert (format "✗ Error loading %s: %s\n" 
                                   feature (error-message-string err)))))))))))
    
    ;; Add summary at the end of the buffer
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert (format "\nSummary: Loaded %d packages with %d errors\n" 
                      loaded-count error-count)))
    
    ;; Display the buffer and show summary in echo area
    (display-buffer log-buffer)
    (message "Loaded %d packages with %d errors. See *Package Loading Log* buffer for details." 
             loaded-count error-count)))
