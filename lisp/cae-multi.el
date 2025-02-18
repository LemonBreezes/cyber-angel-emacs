;;; ~/.doom.d/lisp/cae-multi.el -*- lexical-binding: t; -*-

;; This is code written for the purpose of using this Emacs configuration on
;; multiple machines.

(defvar cae-multi-local-dir (expand-file-name "shared-local/" doom-user-dir))
(defvar cae-multi-data-dir (expand-file-name "etc/" cae-multi-local-dir))
(defvar cae-multi-cache-dir (expand-file-name "cache/" cae-multi-local-dir))
(defvar cae-multi-org-dir "~/org/")

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
(after! calendar
  (setq diary-file (concat cae-multi-secrets-dir "diary")))
(after! transient
  (setq transient-values-file (concat cae-multi-data-dir "transient/values.el")))

(after! bookmark
  (setq bookmark-watch-bookmark-file t))

(use-package! git-auto-commit-mode
  :defer t :init
  (autoload 'gac--after-save "git-auto-commit-mode")
  :config
  (setq-default gac-automatically-add-new-files-p nil
                gac-automatically-push-p t)
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
;; current file, and then read the new file and push the changes. This works by
;; not letting `git-auto-commit-mode' clobber your abbrevs without you noticing.
(advice-add #'write-abbrev-file :around #'cae-multi-abbrev-write-file-a)
(after! abbrev
  (setq cae-multi-abbrev--file-mtime (nth 5 (file-attributes abbrev-file-name))))

;;; Sync the repositories

(defvar cae-multi-repositories
  (list doom-user-dir
        cae-multi-org-dir
        cae-multi-secrets-dir
        (getenv "HOME"))
  "List of directories containing Git repositories to sync between machines.")

(defvar cae-multi-enable-auto-pull (eq system-type 'gnu/linux)
  "If non-nil, automatically pull repositories when idle.")

;; Ideally, I would want a dynamic sync time based on the amount of time it
;; takes to sync. But for now, I will just use a fixed time.
(defun cae-multi-sync-repositories-if-idle ()
  (when (> (time-to-seconds (current-idle-time)) 30)
    (cae-multi-sync-repositories)))

(dir-locals-set-class-variables
 'home
 '((nil
    . ((eval
        . (progn
            ;; Do not render `blamer' hints since we use `git-auto-commit-mode'.
            (setq-local blamer--block-render-p t)

            ;; Automatically commit saved files to Git and push them to the
            ;; remote.
            (when (and (buffer-file-name)
                       (require 'git-auto-commit-mode nil t)
                       (require 'vc-git nil t)
                       (file-equal-p (vc-git-root (buffer-file-name))
                                     "~/"))
              (setq-local gac-automatically-add-new-files-p nil)
              (setq-local gac-automatically-push-p t)
              (git-auto-commit-mode 1))
            ))))))

(dir-locals-set-directory-class (getenv "HOME") 'home)

(when cae-multi-enable-auto-pull
  (run-with-timer 60 60 #'cae-multi-sync-repositories-if-idle))
