;;; private/misc-applications/+know-your-http-well.el -*- lexical-binding: t; -*-

(use-package! know-your-http-well
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-lookup-prefix
        (:prefix ("k" . "know-your-http-well")
         "h" #'http-header
         "m" #'http-method
         "r" #'http-relation
         "s" #'http-status-code
         "t" #'media-type)))
