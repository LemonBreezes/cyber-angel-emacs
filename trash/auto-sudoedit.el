;;; trash/auto-sudoedit.el -*- lexical-binding: t; -*-

;; Loading `tramp-sh' is slow, so we have this hook load auto-sudoedit if we
;; need to use sudo on a file before `tramp-sh' is loaded.
(add-hook 'find-file-hook #'cae-auto-sudoedit-maybe-h -1)
(use-package! auto-sudoedit
  :after tramp-sh :config
  (remove-hook 'find-file-hook #'cae-auto-sudoedit-maybe-h)
  (defadvice! cae-auto-sudoedit-file-local-name-a (oldfun dir buffer setup)
    :around #'dirvish-data-for-dir
    (funcall oldfun (tramp-file-local-name dir) buffer setup))
  (auto-sudoedit-mode +1))
