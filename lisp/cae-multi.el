;;; ~/.doom.d/lisp/cae-multi.el -*- lexical-binding: t; -*-

;; This is code written for the purpose of using this Emacs configuration on
;; multiple machines.

(defvar cae-multi-local-dir (expand-file-name "shared-local/" doom-user-dir))
(defvar cae-multi-data-dir (expand-file-name "etc/" cae-multi-local-dir))
(defvar cae-multi-cache-dir (expand-file-name "cache/" cae-multi-local-dir))
(defvar cae-multi-secrets-dir (expand-file-name "secrets/" doom-user-dir))

(make-directory cae-multi-local-dir t)
(make-directory cae-multi-data-dir t)
(make-directory cae-multi-cache-dir t)
(make-directory cae-multi-secrets-dir t)

(dolist (file (mapcar (doom-partial #'string-remove-suffix ".el")
                      (directory-files cae-multi-secrets-dir t "\\.el$")))
  (load file nil t))

(setq abbrev-file-name (concat cae-multi-data-dir "abbrev_defs"))
(after! bookmark
  (setq bookmark-default-file (concat cae-multi-secrets-dir "bookmarks")))
(after! calc
  (setq calc-settings-file (concat cae-multi-data-dir "calc.el")))
(after! eww
  (setq eww-bookmarks-directory (concat cae-multi-data-dir "eww-bookmarks/")
        eww-download-directory (expand-file-name "~/Downloads/")))
(after! ispell
  (setopt ispell-complete-word-dict (concat cae-multi-data-dir
                                            "dictionaries/word.txt")
          ispell-personal-dictionary (concat cae-multi-secrets-dir
                                             "aspell.en.pws")))

(use-package! git-auto-commit-mode
  :defer t
  :init
  (autoload 'gac--after-save "git-auto-commit-mode")
  :config
  (setq-hook! 'git-auto-commit-mode-hook
    backup-inhibited t))

(advice-add #'write-abbrev-file :after #'cae-multi-abbrev-push-changes-a)
(advice-add #'bookmark-set-internal :after #'cae-multi-bookmark-push-changes-a)
(after! org
  (add-hook 'org-archive-hook #'cae-multi-org-archive-push-changes-h))
