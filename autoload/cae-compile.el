;;; autoload/cae-compile.el -*- lexical-binding: t; -*-

(defvar cae-compile-interesting-file-name-regexp
  "[a-zA-Z0-9-_+]+")

(defvar cae-compile--exit-code nil)
(defun cae-compile-store-exit-code-a (&optional exit-code _)
  (setq cae-compile--exit-code (or exit-code 0)))
(advice-add #'kill-emacs :before #'cae-compile-store-exit-code-a)

;;;###autoload
(defun cae-compile-this-elisp-file ()
  (unless (or no-byte-compile
              (not (stringp (buffer-file-name)))
              (file-in-directory-p (buffer-file-name) doom-local-dir)
              (file-in-directory-p (buffer-file-name) cae-multi-local-dir)
              (not (string-match-p cae-compile-interesting-file-name-regexp
                                   (file-name-base (buffer-file-name)))))
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
(defun +straight-rebuild-package-maybe ()
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
