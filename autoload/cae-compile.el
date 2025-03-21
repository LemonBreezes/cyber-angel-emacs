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
