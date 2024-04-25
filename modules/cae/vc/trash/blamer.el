(use-package! blamer
  :defer t :init
  (map! :leader :desc "Blamer" "tB" #'blamer-mode)
  :config
  (setq blamer-idle-time 0.3
        blamer-avatar-folder (concat doom-cache-dir "avatars/"))
  (setq blamer-author-formatter "%s ")
  (setq blamer-datetime-formatter "[%s]")
  (setq blamer-commit-formatter " ● %s"))
