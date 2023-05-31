;;; Directory Local Variables  -*- lexical-binding: t; -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil
  . ((eval
      . (progn
          (when (and (derived-mode-p 'emacs-lisp-mode)
                     (fboundp '+compile-this-elisp-file))
            (add-hook 'after-save-hook #'+compile-this-elisp-file nil t))
          (when (and (buffer-file-name)
                     (not (file-in-directory-p (buffer-file-name)
                                               (concat doom-private-dir "secrets/")))
                     (require 'git-auto-commit-mode nil t))
            (git-auto-commit-mode 1)
            (setq-local gac-automatically-push-p t))))))
 (magit-status-mode . ((magit-todos-exclude-globs . (".git/" "misc-files/UnicodeData.txt" "shared-local/")))))


;; Local Variables:
;; no-byte-compile: t
;; End:
