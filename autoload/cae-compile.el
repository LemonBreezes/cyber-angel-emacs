;;; autoload/cae-compile.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-compile-this-elisp-file ()
  (unless (or no-byte-compile
              (not (stringp (buffer-file-name)))
              (file-in-directory-p (buffer-file-name) doom-local-dir)
              (file-in-directory-p (buffer-file-name) doom-shared-local-dir)
              (not (string-match-p +compile-interesting-file-name-regexp
                                   (file-name-base (buffer-file-name)))))
    (byte-compile-file (buffer-file-name))
    (emacs-lisp-native-compile-and-load)))
