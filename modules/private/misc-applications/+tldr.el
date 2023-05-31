;;; private/misc-applications/tldr.el -*- lexical-binding: t; -*-

(use-package! tldr
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-lookup-prefix
        "t" #'tldr)
  :config
  (setq tldr-directory-path
        (expand-file-name "tldr" doom-data-dir)))
