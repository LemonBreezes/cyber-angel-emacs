;;; private/misc-applications/+x86-lookup.el -*- lexical-binding: t; -*-

;; All the lookup packages got removed because ChatGPT essentially made them
;; obsolete.

(use-package! x86-lookup
  :defer t
  :init
  (map! :map +misc-applications-lookup-map
        "x" #'x86-lookup)
  :config
  (setq
   x86-lookup-pdf (expand-file-name "325383-sdm-vol-2abcd.pdf" doom-data-dir)
   x86-lookup-cache-directory (expand-file-name "x86-lookup" doom-cache-dir)))
