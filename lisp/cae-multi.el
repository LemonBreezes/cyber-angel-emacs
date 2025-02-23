;;; ~/.doom.d/lisp/cae-multi.el -*- lexical-binding: t; -*-

;; This is code written for the purpose of using this Emacs configuration on
;; multiple machines.

(defvar cae-multi-local-dir (expand-file-name "shared-local/" cae-multi-secrets-dir))
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
(setq cape-dict-file (expand-file-name "en.dic" cae-multi-data-dir))
(after! eww
  (setq eww-bookmarks-directory (concat cae-multi-data-dir "eww-bookmarks/")
        eww-download-directory (expand-file-name "~/Downloads/")))
(after! ispell
  (setq ispell-complete-word-dict (concat cae-multi-data-dir "en.dic")
        ispell-personal-dictionary (concat cae-multi-secrets-dir "aspell.en.pws")))
(after! spell-fu
  (setq spell-fu-directory (concat cae-multi-data-dir "spell-fu/")))
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

(defvar cae-multi-abbrev--auto-commit-disabled nil
  "Non-nil means that automatic saving of abbrev file is temporarily disabled.")

(defmacro with-abbrev-auto-save-disabled (&rest body)
  "Execute BODY with automatic saving of the abbrev file disabled."
  `(let ((cae-multi-abbrev--auto-commit-disabled t))
     ,@body))

(defun cae-multi--disable-auto-save-handler (orig-fun &rest args)
  "Run ORIG-FUN with automatic abbrev file saving disabled.
ARGS are passed on to ORIG-FUN.  This prevents the advise on
`define-abbrev' from scheduling a save during bulk operations such as
reading the abbrev file or defining an abbrev table."
  (with-abbrev-auto-save-disabled
   (apply orig-fun args)))

(defun cae-multi-auto-save-abbrev (&rest _args)
  "Automatically schedule saving the abbrev file after a new abbrev is defined.
This function is meant to be used as :after advice on `define-abbrev'.
It does nothing if `cae-multi-abbrev--auto-commit-disabled' is non-nil."
  (unless cae-multi-abbrev--auto-commit-disabled
    (cae-multi--schedule-auto-save-abbrev))
  nil)

(advice-add #'define-abbrev :after #'cae-multi-auto-save-abbrev)
(advice-add 'read-abbrev-file :around #'cae-multi--disable-auto-save-handler)
(advice-add 'define-abbrevs :around #'cae-multi--disable-auto-save-handler)

(when (eq system-type 'gnu/linux)
  (run-with-idle-timer 5 nil #'cae-multi-start-abbrev-watch))
