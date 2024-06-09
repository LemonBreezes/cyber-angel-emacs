;;; cae/denote/config.el -*- lexical-binding: t; -*-

(use-package! denote
  :hook (dired-mode . denote-dired-mode)
  :defer t :init
  (defvar denote-directory (file-truename "~/org/denote/"))
  (setq denote-known-keywords '(emacs journal ideas learning gentoo projects))

  (map! :leader
        (:prefix-map ("d" . "denote")
         :desc "Dired on notes directory" "d" #'+denote-open-denote-directory
         :desc "New note file" "n" #'denote
         :desc "Find note file" "f" #'+denote-find-note-file
         :desc "Open Denote menu" "m" #'denote-menu-list-notes
         (:prefix-map ("r" . "rename")
          :desc "Using front matter" "f" #'denote-rename-file-using-front-matter)
         (:prefix-map ("l" . "link")
          :desc "Insert link" "l" #'denote-insert-link
          :desc "Insert matching links" "r" #'denote-link-insert-links-matching-regexp)
         (:prefix-map ("k" . "add")
          :desc "Add keywords" "a" #'denote-keywords-add
          :desc "Remove keywords" "r" #'denote-keywords-remove))))

(use-package! denote-explore
  :defer t)

(use-package! denote-menu
  :defer t)
