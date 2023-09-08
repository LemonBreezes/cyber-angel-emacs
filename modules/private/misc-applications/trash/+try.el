;;; private/misc-applications/+try.el -*- lexical-binding: t; -*-

;; I moved `try' into my normal configuration and added it to my
;; `embark-package-map'.

(use-package! try
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "t" #'try)
  (advice-add #'try :before
              (defun +try-package-refresh-contents-maybe (&rest _)
                (unless package-archive-contents
                  (package--archives-initialize)))))
