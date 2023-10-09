;;; private/misc-applications/+devdocs.el -*- lexical-binding: t; -*-

;; All the lookup packages got removed because ChatGPT essentially made them
;; obsolete.

(use-package! devdocs
  :defer t
  :init
  (map! :map +misc-applications-lookup-map
        (:prefix "d"
         "p" #'devdocs-peruse
         "i" #'devdocs-install
         "d" #'devdocs-delete
         "s" #'devdocs-lookup
         "w" #'devdocs-search
         "u" #'devdocs-update-all))
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-lookup-map
      "d" "devdocs"
      "dp" "Read from first page"
      "di" "Install doc"
      "dd" "Delete doc"
      "ds" "Search doc"
      "dw" "Search devdocs website"
      "du" "Update all docs"))
  :config
  (setq devdocs-data-dir (expand-file-name "devdocs" doom-data-dir))
  (map! :map devdocs-mode-map
        "o" #'link-hint-open-link))
