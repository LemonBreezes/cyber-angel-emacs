;;; early-init.el -*- lexical-binding: t; -*-

(defun cae-exclude-file-from-compilation (file-path)
  "Add FILE-PATH to the list of files excluded from native compilation."
  (unless (member file-path compile-angel-excluded-files)
    (add-to-list 'compile-angel-excluded-files file-path)))

(defun cae-exclude-filename-from-compilation (filename)
  "Add FILENAME (just the basename with a leading slash) to excluded files."
  (let ((name (concat "/" (file-name-nondirectory filename))))
    (cae-exclude-file-from-compilation name)))

(defun cae-setup-compile-angel-exclusions ()
  "Set up exclusions for native compilation."
  ;; Exclude system directories/files from native compilation (idempotent)
  (dolist (path '("/usr/share/emacs/31.0.50/lisp/international/"
                  "/usr/share/emacs/31.0.50/lisp/leim/"
                  "/usr/share/emacs/31.0.50/lisp/subdirs.el"
                  "/usr/share/emacs/site-lisp/subdirs.el"))
    (cae-exclude-file-from-compilation path))

  ;; Exclude various configuration files
  (with-eval-after-load "savehist"
    (cae-exclude-filename-from-compilation savehist-file))

  (with-eval-after-load "recentf"
    (cae-exclude-filename-from-compilation recentf-save-file))

  (with-eval-after-load "cus-edit"
    (when (stringp custom-file)
      (cae-exclude-filename-from-compilation custom-file))))

(defun cae-setup-compile-angel (compile-angel-path)
  "Set up compile-angel for native compilation."
  (setq compile-angel-verbose t)
  (add-to-list 'load-path compile-angel-path)
  (require 'compile-angel)
  (compile-angel-on-load-mode +1)

  ;; Native compilation settings
  (setq native-comp-async-query-on-exit t
        confirm-kill-processes t
        package-native-compile t)

  ;; Set up exclusions
  (cae-setup-compile-angel-exclusions))

;; Initialize compile-angel if available
;; This is a workaround for when packages are not compiled
(when-let* ((compile-angel-path (concat doom-local-dir
                                        (format "straight/build-%s/compile-angel/"
                                                emacs-version)))
            (_ (file-exists-p compile-angel-path)))
  (cae-setup-compile-angel compile-angel-path))

;; Local Variables:
;; after-save-hook: cae-doom-patch-emacs-early-init
;; End:
