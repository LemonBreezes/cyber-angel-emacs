;;; early-init.el -*- lexical-binding: t; -*-

;; When this file is loaded, modify ~/.config/emacs/early-init.el to load this file first
(defvar cae-emacs-early-init-file "~/.config/emacs/early-init.el"
  "Path to the Emacs early-init.el file.")

(defvar cae-emacs-early-init-mtime nil
  "Modification time of early-init.el when it was last patched.")

(defun cae-doom-patch-emacs-early-init ()
  "Ensures ~/.config/emacs/early-init.el loads ~/.config/doom/early-init.el first."
  (when (file-exists-p cae-emacs-early-init-file)
    (with-temp-file cae-emacs-early-init-file
      (insert-file-contents cae-emacs-early-init-file)
      (let ((found (re-search-forward "\\((load \"~/.config/doom/early-init.el\")\\)\\|\\(doom-initialize\\)" nil t)))
        (when (and found (match-beginning 2))
          (beginning-of-line)
          (insert "(progn (load \"~/.config/doom/early-init.el\")\n")
          (end-of-line)
          (insert ")"))))
    ;; Store the modification time after patching
    (setq cae-emacs-early-init-mtime
          (nth 5 (file-attributes cae-emacs-early-init-file)))))

(defun cae-check-and-repatch-early-init-h ()
  "Check if early-init.el was modified since last patched and repatch if needed."
  (when (and cae-emacs-early-init-mtime
             (file-exists-p cae-emacs-early-init-file))
    (let ((current-mtime (nth 5 (file-attributes cae-emacs-early-init-file))))
      (when (and current-mtime
                 (time-less-p cae-emacs-early-init-mtime current-mtime))
        (message "Re-patching modified early-init.el before exit")
        (cae-doom-patch-emacs-early-init)))))

;; Add hook to check and repatch early-init.el before exiting
(add-hook 'kill-emacs-hook #'cae-check-and-repatch-early-init-h)

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

;; Function to test if regexps match the intended files
;e Uncomment to test:
;; (defun cae-test-compile-angel-exclusions ()
;;   "Test if the regexps match the files we want to exclude."
;;   (let ((test-files '("/usr/share/emacs/31.0.50/lisp/leim/leim-list.el"
;;                       "/usr/share/emacs/31.0.50/lisp/international/uni-special-titlecase.el"
;;                       "/usr/share/emacs/31.0.50/lisp/international/uni-special-uppercase.el"
;;                       "/usr/share/emacs/31.0.50/lisp/international/uni-titlecase.el"
;;                       "/usr/share/emacs/31.0.50/lisp/international/uni-special-lowercase.el"
;;                       "/usr/share/emacs/31.0.50/lisp/international/uni-lowercase.el"
;;                       "/usr/share/emacs/31.0.50/lisp/international/uni-uppercase.el"
;;                       "/usr/share/emacs/31.0.50/lisp/international/uni-category.el"
;;                       "/usr/share/emacs/31.0.50/lisp/international/uni-brackets.el"
;;                       "/usr/share/emacs/31.0.50/lisp/international/uni-mirrored.el"
;;                       "/usr/share/emacs/31.0.50/lisp/international/uni-bidi.el"
;;                       "~/.config/emacs/.local/etc/@/init.31.0.el"))
;;         (regexps '("/leim/leim-list\\.el$"
;;                    "/international/uni-[^/]+\\.el$"
;;                    "/@/init\\.[0-9]+\\.[0-9]+(\\..*)?\\el$"))
;;         (results nil))
;;     (dolist (file test-files)
;;       (let ((matched nil))
;;         (dolist (regexp regexps)
;;           (when (string-match-p regexp file)
;;             (setq matched t)))
;;         (push (cons file matched) results)))
;;     (let ((unmatched (cl-remove-if #'cdr results)))
;;       (if unmatched
;;           (message "Warning: Some files not matched by regexps: %S" 
;;                    (mapcar #'car unmatched))
;;         (message "All files matched successfully!")))))

(defun cae-ensure-emacs-dir-writable ()
  "Check if /usr/share/emacs is writable and attempt to make it writable if not."
  (let ((emacs-dir "/usr/share/emacs"))
    (when (and (file-exists-p emacs-dir)
               (not (file-writable-p emacs-dir)))
      (message "Warning: %s is not writable. Native compilation may fail." emacs-dir)
      (when (yes-or-no-p (format "Attempt to make %s writable with sudo? " emacs-dir))
        (let ((sudo-cmd (format "sudo chmod -R u+w %s" emacs-dir)))
          (message "Running: %s" sudo-cmd)
          (if (zerop (shell-command sudo-cmd))
              (message "Successfully made %s writable." emacs-dir)
            (message "Failed to make %s writable. You may need to run: %s" 
                     emacs-dir sudo-cmd)))))))

(defun cae-setup-compile-angel (compile-angel-path)
  "Set up compile-angel for native compilation."
  ;; Ensure emacs directory is writable for native compilation
  (cae-ensure-emacs-dir-writable)
  
  (add-to-list 'load-path compile-angel-path)
  (require 'compile-angel)

  ;; Set up exclusions
  (cae-setup-compile-angel-exclusions)

  (compile-angel-on-load-mode +1)

  ;; Native compilation settings
  (setq native-comp-async-query-on-exit t
        confirm-kill-processes t
        package-native-compile t))

;; Initialize compile-angel if available
;; This is a workaround for when packages are not compiled
(when-let* ((compile-angel-path (concat doom-local-dir
                                        (format "straight/build-%s/compile-angel/"
                                                emacs-version)))
            (_ (file-exists-p compile-angel-path)))
  (cae-setup-compile-angel compile-angel-path))

;; Store initial modification time when this file is loaded
(when (and (boundp 'cae-emacs-early-init-file)
           (file-exists-p cae-emacs-early-init-file))
  (setq cae-emacs-early-init-mtime
        (nth 5 (file-attributes cae-emacs-early-init-file))))

;; Local Variables:
;; after-save-hook: cae-doom-patch-emacs-early-init
;; End:
