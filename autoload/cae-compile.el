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
(defun cae-compile-autoloads ()
  (directory-files-recursively
   doom-user-dir
   "[a-zA-Z0-9]+\\.el$"
   nil
   (lambda (s)
     (and
      (cae-compile-file-not-in-unused-module-p s)
      (not
       (cl-member s '("eshell" "packages" "snippets" ".local" ".git"
                      "shared-local" "media" "secrets" "trash")
                  :test (lambda (x y)
                          (string= (file-name-nondirectory x)
                                   y))))))))
