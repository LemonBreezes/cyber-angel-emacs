;;; private/lisp/config.el -*- lexical-binding: t; -*-

(use-package! nameless
  :hook (emacs-lisp-mode . nameless-mode)
  :config
  (setq nameless-private-prefix t
        nameless-global-aliases '()))

(use-package! outline-minor-faces
  :hook (emacs-lisp-mode . outline-minor-faces-add-font-lock-keywords))

(add-hook 'emacs-lisp-mode-hook #'+check-parens-before-save-h)
(add-hook 'lisp-mode-hook #'+check-parens-before-save-h)
(add-hook 'scheme-mode-hook #'+check-parens-before-save-h)
(add-hook 'clojure-mode-hook #'+check-parens-before-save-h)
(add-hook 'racket-mode-hook #'+check-parens-before-save-h)
(add-hook 'lfe-mode-hook #'+check-parens-before-save-h)
(add-hook 'hy-mode-hook #'+check-parens-before-save-h)
(add-hook 'dune-mode-hook #'+check-parens-before-save-h)
(add-hook 'fennel-mode-hook #'+check-parens-before-save-h)
