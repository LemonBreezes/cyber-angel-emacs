;;; Directory Local Variables  -*- lexical-binding: t; -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil
  . ((eval
      . (progn
          (when (and (derived-mode-p 'emacs-lisp-mode)
                     (buffer-file-name)
                     (not (cl-member (file-name-nondirectory (buffer-file-name))
                                     '("init.el"
                                       ".dir-locals.el"
                                       "packages.el")
                                     :test #'string=)))
            (add-hook 'write-file-functions 'eval-buffer 1 t))
          (eval `(after! diff-hl
                   (setf (buffer-local-value 'diff-hl-reference-revision
                                                ,(current-buffer))
                         "HEAD~15")))
          (when (bound-and-true-p cae-config-finished-loading)
            (when (and (derived-mode-p 'emacs-lisp-mode)
                       (fboundp 'cae-compile-this-elisp-file))
              (add-hook 'after-save-hook #'cae-compile-this-elisp-file nil t))
            (when (and (buffer-file-name)
                       (not (file-in-directory-p (buffer-file-name)
                                                 (concat doom-private-dir "secrets/")))
                       (require 'git-auto-commit-mode nil t))
              (git-auto-commit-mode 1)
              (setq-local gac-automatically-push-p t)))))))
 (magit-status-mode . ((magit-todos-exclude-globs . (".git/" "shared-local/")))))
