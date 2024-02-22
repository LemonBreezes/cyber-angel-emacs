;;; Directory Local Variables  -*- lexical-binding: t; -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil
  . ((eval
      . (progn
          (unless (fboundp 'cae-display-graphic-p)
            (defalias 'cae-display-graphic-p 'display-graphic-p))

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
                     (not (string-match-p "/trash/"
                                          buffer-file-name))
                     (not (string-match-p "/benchmarks/"
                                          buffer-file-name))
                     (not (string-match-p "/vanilla-emacs-configs/"
                                          buffer-file-name))
                     (bound-and-true-p cae-config-finished-loading))
            (add-hook 'write-file-functions 'eval-buffer 1 t))

          (when (functionp 'apheleia-mode)
            (apheleia-mode +1))

          ;; Do not render `blamer' hints since we use `git-auto-commit-mode'.
          (setq-local blamer--block-render-p t)

          ;; Automatically update Eshell aliases.
          (when (and (buffer-file-name) (bound-and-true-p eshell-aliases-file)
                     (file-equal-p (buffer-file-name)
                                   (bound-and-true-p eshell-aliases-file))
                     (fboundp 'eshell-read-aliases-list))
            (add-hook 'after-save-hook #'eshell-read-aliases-list nil t))

          (setq-local jinx-local-words "cae corfu eshell")

          ;; Automatically commit saved files to Git and push them to the
          ;; remote.
          (when (and (buffer-file-name)
                     (not (file-in-directory-p (buffer-file-name)
                                               (concat doom-user-dir "secrets/")))
                     (require 'git-auto-commit-mode nil t))
            (git-auto-commit-mode 1)
            (setq-local gac-automatically-push-p t))))))
 (magit-status-mode . ((magit-todos-exclude-globs . (".git/" "shared-local/")))))
