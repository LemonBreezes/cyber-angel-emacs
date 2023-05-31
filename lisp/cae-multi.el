;;; lisp/cae-multi.el -*- lexical-binding: t; -*-

;; This is code written for the purpose of using this Emacs configuration on
;; multiple machines.  It is not intended to be used by anyone else.

(defvar cae-multi-local-dir (expand-file-name "shared-local/" doom-user-dir))
(defvar cae-multi-data-dir (expand-file-name "etc/" cae-multi-local-dir))
(defvar cae-multi-cache-dir (expand-file-name "cache/" cae-multi-local-dir))

(make-directory cae-multi-local-dir t)
(make-directory cae-multi-data-dir t)
(make-directory cae-multi-cache-dir t)

(setq! abbrev-file-name (concat cae-multi-data-dir "abbrev_defs"))
(after! bookmark
  (setq! bookmark-default-file (concat cae-multi-data-dir "bookmarks")))
(after! calc
  (setq! calc-settings-file (concat cae-multi-data-dir "calc.el")))
(after! eww
  (setq! eww-bookmarks-directory (concat cae-multi-data-dir "eww-bookmarks/")
         eww-download-directory (expand-file-name "~/Downloads/")))
(after! ispell
  (setq! ispell-complete-word-dict (concat cae-multi-data-dir
                                           "dictionaries/word.txt")
         ispell-personal-dictionary (concat cae-multi-data-dir
                                            "aspell.en.pws")))

(use-package! git-auto-commit-mode
  :defer t
  :config
  (setq-hook! 'git-auto-commit-mode-hook
    backup-inhibited t))

(defun cae-multi-abbrev-push-changes-a (&optional file _)
  (require 'git-auto-commit-mode)
  (let ((gac-automatically-push-p t)
        (gac-silent-message-p t))
    (gac--after-save (find-file-noselect (if file file abbrev-file-name)))))
(advice-add #'write-abbrev-file :after #'cae-multi-abbrev-push-changes-a)

(defun cae-multi-bookmark-push-changes-a (&rest _)
  (require 'git-auto-commit-mode)
  (let ((gac-automatically-push-p t)
        (gac-silent-message-p t))
    (gac--after-save (find-file-noselect bookmark-default-file))))
(advice-add #'bookmark-set-internal :after #'cae-multi-bookmark-push-changes-a)

(defun cae-multi-org-archive-push-changes-h ()
  (when (string-prefix-p doom-user-dir (buffer-file-name))
    (let ((gac-automatically-push-p t)
          (gac-silent-message-p t))
      (gac--after-save (buffer-file-name))
      (dolist (file (org-all-archive-files))
        (gac--after-save file)))))
(after! org
  (add-hook 'org-archive-hook #'cae-multi-org-archive-push-changes-h))
