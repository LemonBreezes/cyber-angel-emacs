;;; private/misc-applications/+devdocs.el -*- lexical-binding: t; -*-

(use-package! devdocs
  :defer t
  :init
  (map! :leader
        (:prefix +misc-applications-lookup-prefix
         (:prefix ("d" . "devdocs")
          :desc "Read from first page" "p" #'devdocs-peruse
          :desc "Install doc" "i" #'devdocs-install
          :desc "Delete doc" "d" #'devdocs-delete
          :desc "Search doc" "l" #'devdocs-lookup
          :desc "Search devdocs website" "s" #'devdocs-search
          :desc "Update all docs" "u" #'devdocs-update-all)))
  :config
  (setq devdocs-data-dir (expand-file-name "devdocs" doom-data-dir))
  (map! :map devdocs-mode-map
        "o" #'link-hint-open-link))
