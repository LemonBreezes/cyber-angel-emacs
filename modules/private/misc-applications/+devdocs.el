;;; private/misc-applications/+devdocs.el -*- lexical-binding: t; -*-

(use-package! devdocs
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-lookup-prefix
        "d" #'devdocs-lookup)
  :config
  (setq devdocs-data-dir (expand-file-name "devdocs" doom-data-dir))
  (map! :map devdocs-mode-map
        "o" #'link-hint-open-link))
