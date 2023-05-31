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
            (lambda()
              (when (bound-and-true-p flycheck-mode)
                ;; Flycheck will emit errors because the code does not have any
                ;; import/include/etc statements. Besides, we can't use Flycheck
                ;; in an interview anyway.
                (flycheck-mode -1))
              ;; Copilot is basically cheating, so disable it too.
              (when (bound-and-true-p copilot-mode)
                (copilot-mode -1))))
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/src/leetcode"))
