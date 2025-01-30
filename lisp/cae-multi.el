;;; ~/.doom.d/lisp/cae-multi.el -*- lexical-binding: t; -*-

;; This is code written for the purpose of using this Emacs configuration on
;; multiple machines.

(defvar cae-multi-local-dir (expand-file-name "shared-local/" doom-user-dir))
(defvar cae-multi-data-dir (expand-file-name "etc/" cae-multi-local-dir))
(defvar cae-multi-cache-dir (expand-file-name "cache/" cae-multi-local-dir))

(defvar cae-multi-repositories
  (list doom-user-dir
        cae-multi-data-dir
        cae-multi-secrets-dir)
  "List of directories containing Git repositories to pull.")

(make-directory cae-multi-local-dir t)
(make-directory cae-multi-data-dir t)
(make-directory cae-multi-cache-dir t)

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

(defvar cae-multi-enable-auto-pull t
  "If non-nil, automatically pull repositories when idle.")

(when cae-multi-enable-auto-pull
  (run-with-idle-timer 600 t #'cae-multi-pull-repositories))

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

(defun cae-multi-pull-repositories ()
  "Pull the shared repositories and handle conflicts."
  (dolist (repo-dir cae-multi-repositories)
    (let ((default-directory repo-dir))
      (when (file-directory-p (concat repo-dir "/.git"))
        (if (file-exists-p (concat repo-dir "/.git/index.lock"))
            (message "Git lockfile exists in %s, skipping pull" repo-dir)
          (with-temp-buffer
            (let ((exit-code (call-process "git" nil (current-buffer) nil "pull")))
              (if (/= exit-code 0)
                  (progn
                    (message "Git pull failed in %s with exit code %d" repo-dir exit-code)
                    (display-buffer (current-buffer)))
                (goto-char (point-min))
                (if (re-search-forward "CONFLICT" nil t)
                    (progn
                      (message "Conflict detected during git pull in %s" repo-dir)
                      (display-buffer (current-buffer)))
                  (message "Git pull succeeded in %s" repo-dir))))))))))
