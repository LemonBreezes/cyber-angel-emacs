;;; Directory Local Variables  -*- lexical-binding: t; -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil
  . ((eval
      . (progn
          ;; Prevent an Elisp file from being saved if it contains an error.
          ;; This forces us to write our config so that it's idempotent, as it
          ;; will get reloaded on every save.
          (when (and (derived-mode-p 'emacs-lisp-mode)
                     (buffer-file-name)
                     (not (cl-member (file-name-nondirectory (buffer-file-name))
                                     `("init.el"
                                       ,dir-locals-file
                                       "custom.el"
                                       "packages.el")
                                     :test #'string=))
                     (not (string-match-p "/packages/"
                                          buffer-file-name))
                     (bound-and-true-p cae-config-finished-loading))
            (add-hook 'write-file-functions 'eval-buffer 1 t)

            ;; Byte compile autoload files on save.
            (when (member (buffer-file-name)
                          (cae-compile-list-files-to-compile))
              (add-hook 'after-save-hook
                        (lambda ()
                          (byte-recompile-file (buffer-file-name)))
                        nil t)))

          ;; Automatically update Eshell aliases.
          (when (and (buffer-file-name) (bound-and-true-p eshell-aliases-file)
                     (file-equal-p (buffer-file-name)
                                   (bound-and-true-p eshell-aliases-file))
                     (fboundp 'eshell-read-aliases-list))
            (add-hook 'after-save-hook #'eshell-read-aliases-list nil t))

          (setq-local jinx-local-words
                      "cae corfu eshell")

          ;; Automatically compile Emacs Lisp files (if enabled).
          (when (bound-and-true-p cae-config-finished-loading)
            (when (and (derived-mode-p 'emacs-lisp-mode)
                       (fboundp 'cae-compile-this-elisp-file)
                       (bound-and-true-p cae-config-compilation-enabled))
              (add-hook 'after-save-hook #'cae-compile-this-elisp-file nil t))

            ;; Automatically commit saved files to Git and push them to the
            ;; remote.
            (when (and (buffer-file-name)
                       (not (file-in-directory-p (buffer-file-name)
                                                 (concat doom-private-dir "secrets/")))
                       (require 'git-auto-commit-mode nil t))
              (git-auto-commit-mode 1)
              (setq-local gac-automatically-push-p t)))))))
 (magit-status-mode . ((magit-todos-exclude-globs . (".git/" "shared-local/")))))
