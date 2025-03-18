;;; lisp/cae-compile.el -*- lexical-binding: t; -*-

(defun cae-setup-compile-angel-exclusions ()
  "Set up exclusions for native compilation."
  ;; Basic exclusions
  (add-to-list 'compile-angel-excluded-files "/early-init.el")
  (add-to-list 'compile-angel-excluded-files "/subdirs.el")

  ;; Files that fail to compile
  (add-to-list 'compile-angel-excluded-files "/aidermacs-backends.el")

  ;; Exclude various configuration files
  (with-eval-after-load "savehist"
    (add-to-list 'compile-angel-excluded-files
                 (concat "/" (file-name-nondirectory savehist-file))))

  (with-eval-after-load "recentf"
    (add-to-list 'compile-angel-excluded-files
                 (concat "/" (file-name-nondirectory recentf-save-file))))

  (with-eval-after-load "cus-edit"
    (when (stringp custom-file)
      (add-to-list 'compile-angel-excluded-files
                   (concat "/" (file-name-nondirectory custom-file)))))

  ;; Exclude Unicode data files that don't need compilation
  (add-to-list 'compile-angel-excluded-files-regexps "/leim/.*")
  (add-to-list 'compile-angel-excluded-files-regexps "/international/.*")

  ;; Exclude Doom's generated init file
  (add-to-list 'compile-angel-excluded-files-regexps "/@/.*")

  ;; Exclude my site-start file. Gentoo uses it to load site packages.
  (add-to-list 'compile-angel-excluded-files "/site-start.el"))

(defun cae-ensure-emacs-dir-writable ()
  "Check if /usr/share/emacs is writable and attempt to make it writable if not."
  (let ((emacs-dir "/usr/share/emacs"))
    (when (and (file-exists-p emacs-dir)
               (not (file-writable-p emacs-dir)))
      (let ((sudo-cmd (format "sudo chmod -R u+w %s" emacs-dir)))
        (unless (zerop (shell-command sudo-cmd))
          (message "Warning: %s is not writable. Byte compilation may fail." emacs-dir)
          (message "Failed to make %s writable. You may need to run: %s"
                   emacs-dir sudo-cmd))))))

(use-package! compile-angel
  :defer 10.0 :config
  (setq compile-angel-use-file-index t)
  ;; We already native compile packages asynchronously on load.
  (setq compile-angel-enable-native-compile nil)

  (setq compile-angel-debug t)
  (setq compile-angel-verbose t)

  (cae-ensure-emacs-dir-writable)

  ;; Set up exclusions
  (cae-setup-compile-angel-exclusions)
  (compile-angel-on-load-mode +1))
