;;; private/vc/trash/helm-ls-git.el -*- lexical-binding: t; -*-

(use-package! helm-ls-git
  :when (or (modulep! :completion helm)
            (modulep! :cae helm))
  :defer t :init
  (let ((vc-prefix (if (modulep! :editor evil) "g" "v")))
    (map! :leader
          :prefix vc-prefix
          :desc "Helm LS Git" "h" #'helm-ls-git))
  :config
  (setq helm-ls-git-auto-checkout t
        helm-ls-git-fuzzy-match t
        helm-ls-git-status-command (if (modulep! :tools magit)
                                       'magit-status-setup-buffer
                                     'vc-dir)
        helm-ls-git-log-max-commits "500"))
