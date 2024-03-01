;;; lisp/cae-statistics.el -*- lexical-binding: t; -*-

(use-package! emacs-gc-stats
  :init
  (emacs-gc-stats-mode +1))

(use-package! keyfreq
  :init
  (keyfreq-mode +1))

(use-package! wakatime-mode
  :defer t :init
  (add-hook 'doom-first-file-hook #'global-wakatime-mode)
  :config
  (setq wakatime-cli-path (executable-find "wakatime")))
