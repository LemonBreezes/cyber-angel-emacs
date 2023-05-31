;;; autoload/cae-compile.el -*- lexical-binding: t; -*-

(defvar +compile-interesting-file-name-regexp
  "[a-zA-Z0-9-_+]+")

;;;###autoload
(defun cae-compile-this-elisp-file ()
  (unless (or no-byte-compile
              (not (stringp (buffer-file-name)))
              (file-in-directory-p (buffer-file-name) doom-local-dir)
              (file-in-directory-p (buffer-file-name) cae-multi-local-dir)
              (not (string-match-p +compile-interesting-file-name-regexp
                                   (file-name-base (buffer-file-name)))))
    (byte-compile-file (buffer-file-name))
    (emacs-lisp-native-compile-and-load)))

;;;###autoload
(defun cae-compile-my-private-config ()
  (interactive)
  (when (or (not +kill-emacs--exit-code)
            (eq +kill-emacs--exit-code 0))
    (+compile-pdf-tools))
  (mapc (lambda (s)
          (unless
              (or (string= (file-name-nondirectory s) "packages.el")
                  (string= (file-name-nondirectory s) "doctor.el")
                  (string= (file-name-nondirectory s) ".dir-locals.el")
                  (string-prefix-p "flycheck_" (file-name-nondirectory s))
                  (cl-member s +compile-files-to-ignore :test #'string=)
                  (and +kill-emacs--exit-code
                       (not (eq +kill-emacs--exit-code 0))
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
                                      #'+file-not-in-unused-module-p)
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
