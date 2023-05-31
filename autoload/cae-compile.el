;;; autoload/cae-compile.el -*- lexical-binding: t; -*-

(defvar cae-compile-interesting-file-name-regexp
  "[a-zA-Z0-9-_+]+")

(defvar cae-compile--exit-code nil)
(defun cae-compile-store-exit-code-a (&optional exit-code _)
  (setq cae-compile--exit-code (or exit-code 0)))
(advice-add #'kill-emacs :before #'cae-compile-store-exit-code-a)

(defvar cae-compile-files-to-ignore
  `(;; Compiling these creates errors.
    ;; ,(expand-file-name "lang/emacs-lisp/autoload.el" doom-modules-dir)
    ,(expand-file-name "config/default/config.el" doom-modules-dir)
    ,(expand-file-name "lang/org/autoload/contrib-present.el" doom-modules-dir)
    ,(expand-file-name "doom-start.el" doom-core-dir)

    ;; I have not tested compiling these files but it's better not to compile
    ;; them anyways as they are ran in a CLI and hence are harder to debug.
    ,(expand-file-name "doom-cli.el" doom-core-dir)))

(defun cae-compile--compile-pdf-tools ()
  (unless (ignore-errors (and (require 'pdf-tools nil t)
                              (pdf-info-check-epdfinfo))
                         t)
    (advice-add #'fboundp :around
                (defun tmp/fboundp-a (oldfun function)
                  (unless (eq function 'make-process)
                    (funcall oldfun function))))
    (advice-add #'exwm-input--update-focus-commit :override
                #'ignore)
    (let ((compilation-filter-hook
           (remove 'comint-truncate-buffer compilation-filter-hook)))
      (unwind-protect
          (pdf-tools-build-server (or (and (stringp pdf-info-epdfinfo-program)
                                           (file-name-directory
                                            pdf-info-epdfinfo-program))
                                      pdf-tools-directory)
                                  ;; Use system versions of dependencies.
                                  t)
        (advice-remove #'fboundp #'tmp/fboundp-a)
        (advice-remove #'exwm-input--update-focus-commit #'ignore)))))

(defun cae-compile-file-not-in-unused-module-p (&optional file-name)
  (not (when-let* ((file-name (or file-name (buffer-file-name)))
                   (file-path (or (and (file-directory-p file-name)
                                       (expand-file-name file-name))
                                  (file-name-directory file-name)))
                   (module-dir (cl-find-if (lambda (x) (string-prefix-p x file-path))
                                           doom-modules-dirs))
                   (module (doom-module-from-path file-path)))
         (not (doom-module-p (car module) (cdr module))))))

;;;###autoload
(defun cae-compile-this-elisp-file ()
  (unless (or no-byte-compile
              (not (stringp (buffer-file-name)))
              (file-in-directory-p (buffer-file-name) doom-local-dir)
              (file-in-directory-p (buffer-file-name) cae-multi-local-dir)
              (not (string-match-p cae-compile-interesting-file-name-regexp
                                   (file-name-base (buffer-file-name))))
              (not (string-match-p "flycheck_.*" (buffer-file-name))))
    (byte-compile-file (buffer-file-name))
    (emacs-lisp-native-compile-and-load)))

;;;###autoload
(defun cae-compile-my-private-config ()
  (interactive)
  (mapc (lambda (s)
          (unless
              (or (string= (file-name-nondirectory s) "packages.el")
                  (string= (file-name-nondirectory s) "doctor.el")
                  (string= (file-name-nondirectory s) ".dir-locals.el")
                  (string-prefix-p "flycheck_" (file-name-nondirectory s))
                  (and cae-compile--exit-code
                       (not (eq cae-compile--exit-code 0))
                       (not (file-exists-p (concat s "c"))))
                  (file-newer-than-file-p (concat s "c") s))
            (ignore-errors (byte-compile-file s))
            (ignore-errors (native-compile s))))
        (nconc
         ;; Compiling `lisp/lib' creates some errors and these functions
         ;; are not that important to have compiled anyways.
         (directory-files-recursively doom-core-dir
                                      "[a-za-z0-9]+\\.el$"
                                      nil
                                      #'ignore)
         (directory-files-recursively doom-modules-dir
                                      "[a-za-z0-9]+\\.el$"
                                      nil
                                      #'cae-compile-file-not-in-unused-module-p)
         (directory-files-recursively
          doom-user-dir
          "[a-zA-Z0-9]+\\.el$"
          nil
          (lambda (s)
            (not (or (string= (file-name-nondirectory s) "experiments")
                     (string= (file-name-nondirectory s) "eshell")
                     (string= (file-name-nondirectory s) "misc-files")
                     (string= (file-name-nondirectory s) "snippets")
                     (string= (file-name-nondirectory s) ".local")
                     (string= (file-name-nondirectory s) ".git")
                     (string= (file-name-nondirectory s) "shared-local"))))))))

;;;###autoload
(defun cae-compile-rebuild-package ()
  (when (and (stringp (buffer-file-name))
             (stringp (file-name-directory (buffer-file-name)))
             (string-prefix-p (expand-file-name ".local/straight/" doom-emacs-dir)
                              (file-name-directory (buffer-file-name))))
    (require 'straight)
    (when-let (package (straight--determine-repo buffer-file-name))
      (mapc #'delete-file (directory-files (file-name-directory
                                            (buffer-file-name))
                                           nil
                                           "flycheck_.*"))
      (straight-rebuild-package package))))

(add-hook 'after-save-hook #'cae-compile-rebuild-package)

;; Run early in case I want to `C-g' and inspect the output.
(add-hook 'kill-emacs-hook #'cae-compile-my-private-config -1)

;; Make Emacs way faster at byte compiling but if this threshold is ever hit,
;; you will be waiting for a long time.
(add-hook 'kill-emacs-hook
          ;; 10 GB or 30% of RAM
          (lambda () (setq gc-cons-threshold (* 10 1024 1024 1024)
                      gc-cons-percentage 30
                      gcmh-low-cons-threshold gc-cons-threshold
                      gcmh-high-cons-threshold gc-cons-threshold))
          -10)
