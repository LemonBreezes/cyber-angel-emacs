;;; private/misc-applications/elcord.el -*- lexical-binding: t; -*-

(use-package! elcord
  :hook (doom-first-file . elcord-mode)
  :when (and (cae-display-graphic-p)
             (not (or (memq system-type '(cygwin windows-nt ms-dos))
                      (getenv "SSH_TTY")
                      (getenv "WSL_DISTRO_NAME"))))
  :config
  (setq elcord-quiet t
        elcord-use-major-mode-as-main-icon t
        elcord-refresh-rate 10
        elcord-idle-timer 300
        elcord-idle-message "Going for a walk...")
  (require 'parent-mode)
  (defadvice! elcord--make-process-for-wsl-a (oldfun)
    :around #'elcord--make-process
    (let ((default-directory "~/"))
      (if (getenv "WSL_DISTRO_NAME")
          (make-process
           :name "*elcord-sock*"
           :command (list
                     "powershell.exe"
                     "-NoProfile"
                     "-ExecutionPolicy" "Bypass"
                     "-Command" elcord--stdpipe-path "." elcord--discord-ipc-pipe)
           :connection-type 'pipe
           :sentinel 'elcord--connection-sentinel
           :filter 'elcord--connection-filter
           :noquery t)
        (funcall oldfun))))
  (defadvice! +elcord--buffer-boring-p-a (buffer-name)
    :before-until #'elcord--buffer-boring-p
    (or (string-match-p "^\\*\\(doom\\|Messages\\|scratch\\|dashboard\\|elfeed\\|vterm\\|eshell\\|terminal\\|magit\\|help\\|Compile-Log\\|lsp\\|treemacs\\|\\*\\)" buffer-name)
        (parent-mode-is-derived-p (buffer-local-value 'major-mode (get-buffer buffer-name))
                                  'exwm-mode))))
