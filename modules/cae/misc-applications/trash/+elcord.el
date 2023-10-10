;;; private/misc-applications/trash/+elcord.el -*- lexical-binding: t; -*-

;; This package is pretty cool. But I decided I did not want everyone on Discord
;; to know what I was doing all the time. So I disabled it.

(use-package! elcord
  :when (and (cae-display-graphic-p)
             (not (or (memq system-type '(cygwin windows-nt ms-dos))
                      (getenv "WSL_DISTRO_NAME")))
             ;; I only use this on my desktop machine when EXWM is running.
             (modulep! :cae exwm))
  :defer t :hook (doom-first-file . elcord-mode)
  :init
  (defadvice! +elcord--buffer-boring-p-a (buffer-name)
    :before-until #'elcord--buffer-boring-p
    (or (string-match-p "^\\*\\(doom\\|Messages\\|scratch\\|dashboard\\|elfeed\\|vterm\\|eshell\\|terminal\\|magit\\|help\\|Compile-Log\\|lsp\\|treemacs\\|\\*\\)" buffer-name)
        (and (parent-mode-is-derived-p (buffer-local-value 'major-mode (get-buffer buffer-name))
                                       'exwm-mode)
             (not (string-match-p "LeetCode" buffer-name)))
        (parent-mode-is-derived-p (buffer-local-value 'major-mode (get-buffer buffer-name))
                                  'dired-mode)))
  :config
  (setq elcord-quiet t
        elcord-use-major-mode-as-main-icon t
        elcord-refresh-rate 10
        elcord-idle-timer 300
        elcord-idle-message "Going for a walk...")
  (autoload 'parent-mode-is-derived-p "parent-mode"))
