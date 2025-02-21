((nil
  . ((eval
      . (progn
          ;; Do not render `blamer' hints since we use `git-auto-commit-mode'.
          (setq-local blamer--block-render-p t)

          ;; Automatically commit saved files to Git and push them to the
          ;; remote.
          (when (and (buffer-file-name)
                     (require 'git-auto-commit-mode nil t)
                     (require 'vc-git nil t)
                     (eq (vc-backend (buffer-file-name)) 'Git)
                     (file-equal-p (vc-git-root (buffer-file-name))
                                   "~/"))
            (setq-local gac-automatically-add-new-files-p nil)
            (setq-local gac-automatically-push-p nil)
            (git-auto-commit-mode 1))
          )))))
