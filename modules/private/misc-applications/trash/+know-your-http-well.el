;;; private/misc-applications/+know-your-http-well.el -*- lexical-binding: t; -*-

;; All the lookup packages got removed because ChatGPT essentially made them
;; obsolete.

(use-package! know-your-http-well
  :defer t
  :init
  (map! :map +misc-applications-lookup-map
        (:prefix "k"
         "h" #'http-header
         "m" #'http-method
         "r" #'http-relation
         "s" #'http-status-code
         "t" #'media-type))
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-lookup-map
      "k" "know-your-http-well")))
