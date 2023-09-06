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

(when (file-exists-p (concat cae-multi-secrets-dir "secrets.el"))
  (load! (concat cae-multi-secrets-dir "secrets.el")))

(setq abbrev-file-name (concat cae-multi-data-dir "abbrev_defs"))
(after! bookmark
  (setq bookmark-default-file (concat cae-multi-secrets-dir "bookmarks")))
(after! bbdb
  (setq bbdb-file (concat cae-multi-secrets-dir "bbdb")))
(after! calendar
  (setq diary-file (concat cae-multi-secrets-dir "diary")))
(after! calc
  (setq calc-settings-file (concat cae-multi-data-dir "calc.el")))
(after! eww
  (setq eww-bookmarks-directory (concat cae-multi-data-dir "eww-bookmarks/")
        eww-download-directory (expand-file-name "~/Downloads/")))
(after! ispell
  (setq ispell-complete-word-dict (concat cae-multi-data-dir "en.dic")
        ispell-personal-dictionary (concat cae-multi-secrets-dir "aspell.en.pws")))
(setq cape-dict-file (expand-file-name "en.dic" cae-multi-data-dir))

(use-package! git-auto-commit-mode
  :defer t :init
  (autoload 'gac--after-save "git-auto-commit-mode")
  :config
  (setq-hook! 'git-auto-commit-mode-hook
    backup-inhibited t)

  ;; Disable `diff-hl-mode' in my Doom private dir.
  (defadvice! cae-hacks-disable-diff-hl-in-private-config-a (&optional arg)
    :before-until #'diff-hl-mode
    (file-in-directory-p default-directory doom-user-dir)))

(advice-add #'bookmark-set-internal :after #'cae-multi-bookmark-push-changes-a)
(after! org
  (add-hook 'org-archive-hook #'cae-multi-org-archive-push-changes-h))

;;; Abbrevs

(defvar cae-multi-abbrev--file-mtime nil)
(defvar cae-multi-abbrev--auto-commit-disabled nil)

;; A better idea is to write abbrevs to a temp file, diff the temp file with the
;; current file, and then read the new file and push the changes. Do this with a
;; file watcher.
(advice-add #'write-abbrev-file :around #'cae-multi-abbrev-write-file-a)
(after! abbrev
  (setq cae-multi-abbrev--file-mtime (nth 5 (file-attributes abbrev-file-name))))
