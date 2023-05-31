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
        "q" #'+leetcode-quit))
