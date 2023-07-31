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
  (require 'dash)
  ;; List autoloads and startup program files
  (-filter
   (lambda (s) (or (string-match-p "autoload" s)
              (and (modulep! :private exwm)
                   (string-match-p "/startup-programs/" s)
                   (not (string-match-p "disabled" s)))))
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
                                    y)))))))))

;;;###autoload
(defun cae-compile-private-config ()
  (mapc #'byte-compile-file
        (cae-compile-list-files-to-compile)))
