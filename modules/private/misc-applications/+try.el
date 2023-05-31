;;; private/misc-applications/+try.el -*- lexical-binding: t; -*-

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
