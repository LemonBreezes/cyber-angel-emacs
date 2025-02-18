;;; text.el -*- lexical-binding: t; -*-

(remove-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)

(after! calendar
  (setq calendar-week-start-day 1
        calendar-mark-diary-entries-flag t))

(after! org
  (setq org-directory (or (bound-and-true-p cae-multi-org-dir) "~/org/")
        org-extend-today-until 3
        org-startup-with-inline-images t
        org-startup-align-all-tables t
        org-image-actual-width t
        org-log-done 'time
        org-log-done-with-time t
        org-ellipsis " ..."
        org-archive-location (concat org-directory ".archive/%s::")
        org-hide-emphasis-markers t
        org-special-ctrl-k t
        org-highlight-latex-and-related nil
        org-priority-highest ?A
        org-priority-lowest ?E
        ;; All my computers use 64-bit processors
        org-read-date-force-compatible-dates nil)
  (when (require 'nerd-icons-faces nil t)
    (setq org-priority-faces
          '((?A . nerd-icons-red)
            (?B . nerd-icons-orange)
            (?C . nerd-icons-yellow)
            (?D . nerd-icons-green)
            (?E . nerd-icons-blue))))
  (when (modulep! :lang org +roam2)
    (setq +org-roam-auto-backlinks-buffer nil))
  (map! :map org-mode-map
        "C-c C-M-h" #'er/mark-org-code-block)
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  (after! org-list
    (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")
                                          ("1." . "a."))))
  (after! ob-core
    ;; Export commments by default.
    (setq org-babel-default-header-args
          '((:session . "none")
            (:results . "replace")
            (:exports . "code")
            (:cache . "no")
            (:noweb . "no")
            (:hlines . "no")
            (:tangle . "no")
            (:comments . "link"))))
  (after! ox
    (setq org-export-allow-bind-keywords t))
  (after! org-crypt
    (setq org-crypt-disable-auto-save 'encrypt))
  (after! org-agenda
    (setq org-agenda-sticky nil
          org-agenda-files '("~/org/" "~/org/denote/" "~/org/roam/daily/")))

  (after! which-key
    (which-key-add-keymap-based-replacements org-mode-map
      "C-c \"" "plot"
      "C-c C-v" "org-babel-map")))

(after! org-journal
  (setq org-journal-file-format "%Y-%m-%d.org"))
(when (modulep! :editor evil)
  (add-hook 'org-journal-after-entry-create-hook #'evil-insert-state))

(after! markdown-mode
  (setq markdown-fontify-code-blocks-natively t))
