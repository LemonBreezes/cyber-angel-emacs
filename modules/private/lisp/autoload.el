;;; private/lisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-lisp-check-parens-before-save-h ()
  (add-hook 'write-file-functions 'check-parens nil t))

;;;###autoload
(defun cae-lisp-enable-elisp-mode-in-dir-locals-file-h ()
  (when (and (not (eq major-mode 'emacs-lisp-mode))
             (buffer-file-name)
             (string= (file-name-nondirectory (buffer-file-name))
                      ".dir-locals.el"))
    (emacs-lisp-mode)))
