;;; private/misc-applications/+leetcode.el -*- lexical-binding: t; -*-

(use-package! leetcode
  :defer t
  :init
  (when (memq system-type '(cygwin windows-nt ms-dos))
    (advice-add #'leetcode--install-my-cookie :override #'ignore))
  (map! :leader
        :prefix +misc-applications-prefix
        "L" #'+leetcode)
  :config
  (map! :map leetcode--problems-mode-map
        "q" #'+leetcode-quit
        :map leetcode--problem-detail-mode-map
        "o" #'link-hint-open-link)
  (add-hook 'leetcode-solution-mode-hook
            (lambda() (flycheck-mode -1)))
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/src/leetcode"))
