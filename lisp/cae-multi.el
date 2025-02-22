;;; ~/.doom.d/lisp/cae-multi.el -*- lexical-binding: t; -*-

;; This is code written for the purpose of using this Emacs configuration on
;; multiple machines.

(defvar cae-multi-local-dir (expand-file-name "shared-local/" doom-user-dir))
(defvar cae-multi-data-dir (expand-file-name "etc/" cae-multi-local-dir))
(defvar cae-multi-cache-dir (expand-file-name "cache/" cae-multi-local-dir))
(defvar cae-multi-org-dir (expand-file-name "org/" cae-multi-local-dir))
(defvar cae-multi-secrets-dir (expand-file-name "secrets/" cae-multi-local-dir))

(make-directory cae-multi-local-dir t)
(make-directory cae-multi-data-dir t)
(make-directory cae-multi-cache-dir t)
(make-directory cae-multi-org-dir t)

(after! abbrev
  (setq abbrev-file-name (concat cae-multi-data-dir "abbrev_defs")))
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
(after! transient
  (setq transient-values-file (concat cae-multi-data-dir "transient/values.el")))

(use-package! git-auto-commit-mode
  :defer t :init
  (autoload 'gac--after-save "git-auto-commit-mode")
  :config
  (setq-default gac-automatically-add-new-files-p nil)
  (setq-hook! 'git-auto-commit-mode-hook
    backup-inhibited t))

(defun cae-multi-bookmark-push-changes-a (&rest _)
  (cae-multi--push-changes bookmark-default-file " *cae-multi-bookmark-push-changes-a*"))

(defun cae-multi-org-archive-push-changes-h ()
  (gac--after-save (buffer-file-name))
  (dolist (file (org-all-archive-files))
    (gac--after-save file)))

(setq bookmark-save-flag 1)
(setq bookmark-watch-bookmark-file 'silent)

(advice-add #'bookmark-save :after #'cae-multi-bookmark-push-changes-a)
(after! org
  (add-hook 'org-archive-hook #'cae-multi-org-archive-push-changes-h))

;;; Abbrevs

(defvar cae-multi-abbrev--auto-commit-disabled nil)

;;; Sync the repositories

(defvar cae-multi-repositories
  (list doom-user-dir
        cae-multi-org-dir
        cae-multi-secrets-dir
        (getenv "HOME"))
  "List of directories containing Git repositories to sync between machines.")

(defvar cae-multi-enable-auto-pull (eq system-type 'gnu/linux)
  "If non-nil, automatically pull repositories when idle.")

;; Gotta sync when idle to prevent the sync from interfering with git commands.
(defun cae-multi-sync-repositories-when-idle ()
  (when (> (time-to-seconds (current-idle-time)) 10)
    (cae-multi-sync-repositories)))
(when cae-multi-enable-auto-pull
  (cae-run-with-timer 30 30 "cae-multi-sync-repositories"
                      #'cae-multi-sync-repositories-when-idle))

;;; Hot reloading abbrevs

(defvar cae-multi-abbrev--file-mtime nil)
(after! abbrev
  (setq cae-multi-abbrev--file-mtime (nth 5 (file-attributes abbrev-file-name))))

(defvar cae-multi-abbrev-watch-descriptor nil
  "File notification descriptor for the abbrev file.")

(defun cae-multi-start-abbrev-watch ()
  "Start watching the abbrev file for external changes.
When the abbrev file (given by the variable `abbrev-file-name`) changes,
the abbrevs are reloaded automatically."
  (when (and abbrev-file-name (file-exists-p abbrev-file-name))
    (unless cae-multi-abbrev-watch-descriptor
      (setq cae-multi-abbrev-watch-descriptor
            (file-notify-add-watch
             abbrev-file-name
             '(change)
             #'cae-multi-abbrev-watch-callback)))))

(advice-add #'define-abbrev :after #'cae-multi-auto-save-abbrev)

(when (eq system-type 'gnu/linux)
  (run-with-idle-timer 5 nil #'cae-multi-start-abbrev-watch))
