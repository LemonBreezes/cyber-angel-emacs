;;; lisp/cae-compile.el -*- lexical-binding: t; -*-

(defun cae-exclude-file-from-compilation (file-path)
  "Add FILE-PATH to the list of files excluded from native compilation."
  (unless (member file-path compile-angel-excluded-files)
    (add-to-list 'compile-angel-excluded-files file-path)))

(defun cae-exclude-filename-from-compilation (filename)
  "Add FILENAME (just the basename with a leading slash) to excluded files."
  (let ((name (concat "/" (file-name-nondirectory filename))))
    (cae-exclude-file-from-compilation name)))

(defun cae-exclude-file-regexp-from-compilation (regexp)
  "Add files matching REGEXP to the list of excluded files for native compilation."
  (unless (member regexp compile-angel-excluded-files-regexps)
    (add-to-list 'compile-angel-excluded-files-regexps regexp)))

(defun cae-setup-compile-angel-exclusions ()
  "Set up exclusions for native compilation."
  (cae-exclude-file-from-compilation "/early-init.el")
  (cae-exclude-file-from-compilation "/subdirs.el")

  ;; Exclude various configuration files
  (with-eval-after-load "savehist"
    (cae-exclude-filename-from-compilation savehist-file))

  (with-eval-after-load "recentf"
    (cae-exclude-filename-from-compilation recentf-save-file))

  (with-eval-after-load "cus-edit"
    (when (stringp custom-file)
      (cae-exclude-filename-from-compilation custom-file)))

  ;; Exclude Unicode data files that don't need compilation
  (cae-exclude-file-regexp-from-compilation "/leim/.*")
  (cae-exclude-file-regexp-from-compilation "/international/.*")

  ;; Exclude Doom's generated init file
  (cae-exclude-file-regexp-from-compilation "/@/.*"))

(defun cae-ensure-emacs-dir-writable ()
  "Check if /usr/share/emacs is writable and attempt to make it writable if not."
  (let ((emacs-dir "/usr/share/emacs"))
    (when (and (file-exists-p emacs-dir)
               (not (file-writable-p emacs-dir)))
      (message "Warning: %s is not writable. Byte compilation may fail." emacs-dir)
      (when (yes-or-no-p (format "Attempt to make %s writable with sudo? " emacs-dir))
        (let ((sudo-cmd (format "sudo chmod -R u+w %s" emacs-dir)))
          (message "Running: %s" sudo-cmd)
          (if (zerop (shell-command sudo-cmd))
              (message "Successfully made %s writable." emacs-dir)
            (message "Failed to make %s writable. You may need to run: %s"
                     emacs-dir sudo-cmd)))))))

(use-package! compile-angel
  :defer 60 :config
  (cae-ensure-emacs-dir-writable)

  ;; Set up exclusions
  (cae-setup-compile-angel-exclusions)
  (unless cae-config-finished-loading
    (compile-angel-on-load-mode +1))

  ;; Native compilation settings
  (setq native-comp-async-query-on-exit t
        confirm-kill-processes t
        package-native-compile t))
