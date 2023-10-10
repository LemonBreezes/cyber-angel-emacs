;;; autoload/cae-compile.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-compile-rebuild-package ()
  (when (and (stringp (buffer-file-name))
             (stringp (file-name-directory (buffer-file-name)))
             (string-prefix-p (expand-file-name ".local/straight/" doom-emacs-dir)
                              (file-name-directory (buffer-file-name))))
    (require 'straight)
    (when-let ((package (straight--determine-repo buffer-file-name)))
      (mapc #'delete-file (directory-files (file-name-directory
                                            (buffer-file-name))
                                           nil
                                           "flycheck_.*"))
      (straight-rebuild-package package))))

;;;###autoload
(defun cae-compile-list-files-to-compile ()
  ;; List autoloads and startup program files
  (thread-last
    (directory-files-recursively
     doom-user-dir
     "[a-zA-Z0-9]+\\.el$"
     nil
     (lambda (s)
       (and
        (not
         (cl-member s '("eshell" "packages" "snippets" ".local" ".git"
                        "shared-local" "media" "secrets" "trash" "org" "media"
                        "lisp")
                    :test (lambda (x y)
                            (string= (file-name-nondirectory x)
                                     y)))))))
    (cl-remove-if-not
     (lambda (s) (or (string-match-p "autoload" s)
                (and (string-match-p "/startup-programs/" s)
                     (not (string-match-p "disabled" s))))))
    (cons (concat doom-user-dir "config.el"))
    (cons (concat doom-user-dir "lisp/cae-bindings.el"))
    (cons (doom-module-locate-path :cae 'misc-applications "config.el"))))

;;;###autoload
(defun cae-compile-private-config ()
  (mapc #'byte-recompile-file
        (cae-compile-list-files-to-compile)))
