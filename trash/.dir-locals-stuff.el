;;; trash/.dir-locals-stuff.el -*- lexical-binding: t; -*-


;; Automatically compile Emacs Lisp files (if enabled).
(when (bound-and-true-p cae-config-finished-loading)
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (fboundp 'cae-compile-this-elisp-file)
             (bound-and-true-p cae-config-compilation-enabled))
    (add-hook 'after-save-hook #'cae-compile-this-elisp-file nil t))


  ;; Byte compile autoload files on save.
  (when (and (fboundp 'cae-compile-list-files-to-compile)
             (member (buffer-file-name)
                     (cae-compile-list-files-to-compile)))
    (add-hook 'after-save-hook #'elisp-byte-compile-file nil t)))
