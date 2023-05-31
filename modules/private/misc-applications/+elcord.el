;;; private/misc-applications/elcord.el -*- lexical-binding: t; -*-

(use-package! elcord
  :hook (doom-first-file . elcord-mode)
  :when (and (display-graphic-p)
             (not (or (memq system-type '(cygwin windows-nt ms-dos))
                      (getenv "SSH_TTY"))))
  :config
  (setq! elcord-quiet t
         elcord-use-major-mode-as-main-icon t
         elcord-refresh-rate 10
         elcord-idle-timer 300
         elcord-idle-message "Going for a walk...")
  (require 'parent-mode)
  (defadvice! +elcord--buffer-boring-p-a (buffer-name)
    :before-until #'elcord--buffer-boring-p
    (or (string-match-p "^\\*\\(doom\\|Messages\\|scratch\\|dashboard\\|elfeed\\|vterm\\|eshell\\|terminal\\|magit\\|help\\|Compile-Log\\|lsp\\|treemacs\\|\\*\\)" buffer-name)
        (parent-mode-is-derived-p (buffer-local-value 'major-mode (get-buffer buffer-name))
                                  'exwm-mode))))
