;;; lisp/cae-tty.el -*- lexical-binding: t; -*-

;; Stuff so that Emacs doesn't break in the Terminal.
(when (modulep! :completion vertico +childframe)
  (unless (cae-display-graphic-p)
    (remove-hook 'vertico-mode-hook #'vertico-posframe-mode)))
(when (modulep! :ui ligatures)
  (unless (cae-display-graphic-p)
    (setq +ligatures-in-modes nil)
    (remove-hook 'doom-init-ui-hook #'+ligatures-init-h)
    (remove-hook 'doom-init-ui-hook #'+ligature-init-composition-table-h)
    (remove-hook 'doom-init-ui-hook #'+ligatures-init-buffer-h)))
