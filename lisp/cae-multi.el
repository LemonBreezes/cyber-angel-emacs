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
  (setq ispell-complete-word-dict (concat cae-multi-data-dir
                                          "dictionaries/word.txt")
        ispell-personal-dictionary (concat cae-multi-secrets-dir
                                           "aspell.en.pws")))
(after! cape
  (setq cape-dict-file (expand-file-name "en.dic" cae-multi-data-dir)))

(use-package! git-auto-commit-mode
  :defer t
  :init
  (autoload 'gac--after-save "git-auto-commit-mode")
  :config
  (setq-hook! 'git-auto-commit-mode-hook
    backup-inhibited t))

(advice-add #'bookmark-set-internal :after #'cae-multi-bookmark-push-changes-a)
(after! org
  (add-hook 'org-archive-hook #'cae-multi-org-archive-push-changes-h))

;;; Abbrevs

(advice-add #'write-abbrev-file :after #'cae-multi-abbrev-push-changes-a)

(defvar my-abbrev-file-watcher nil
  "File watcher for the abbrev file.")

(defun my-abbrev-file-watcher-callback (event)
  "React to changes in the abbrev file by removing the `git-auto-commit-mode' hook."
  (let ((event-type (nth 1 event))
        (event-file (nth 2 event)))
    (when (and (eq event-type 'changed)
               (file-equal-p event-file abbrev-file-name))
      (+log "Abbrev file changed, removing git-auto-commit-mode hook"))))

(defun my-setup-abbrev-file-watcher ()
  "Set up a file watcher for the abbrev file."
  (when (and abbrev-file-name
             (file-exists-p abbrev-file-name)
             (null my-abbrev-file-watcher))
    (setq my-abbrev-file-watcher
          (file-notify-add-watch abbrev-file-name
                                 '(change)
                                 #'my-abbrev-file-watcher-callback))))

(add-hook 'abbrev-mode-hook #'my-setup-abbrev-file-watcher)
