;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defvar cae-config-finished-loading nil)

;;; Stuff that should not be disabled.

(when cae-init-core-enabled-p
  (load! "core"))


;;; UI

(when cae-init-ui-enabled-p
  (load! "ui"))


;;; Tools

(when cae-init-tools-enabled-p
  (load! "tools"))


;;; Editor

(when cae-init-editor-enabled-p
  (load! "editor"))


;;; Autocompletion

(when cae-init-autocompletion-enabled-p
  (when (modulep! :completion corfu)
    (load! "lisp/cae-corfu"))

  (setq ido-save-directory-list-file (concat doom-cache-dir "ido.last"))
  (after! ido
    (load! "lisp/cae-ido"))

  (after! yasnippet
    (setq yas-triggers-in-field t       ;Allow nested snippets.
          yas-trigger-symbol " →")
    (setq-hook! 'org-mode-hook
      yas-triggers-in-field nil))

  (use-package! hippie-exp
    :defer t :config
    (setq  hippie-expand-try-functions-list
           '(try-expand-dabbrev
             try-expand-dabbrev-all-buffers
             try-expand-dabbrev-from-kill
             try-complete-file-name-partially
             try-complete-file-name
             try-complete-lisp-symbol-partially
             try-complete-lisp-symbol
             try-expand-line)
           hippie-expand-verbose nil))

  (use-package! consult
    :when (modulep! :completion vertico)
    :defer t :init
    ;; See `lisp/cae-bindings' for keybindings.
    :config
    (consult-customize
     consult-theme :preview-key '(:debounce 0.2 any))
    (setq consult-preview-key
          '(:debounce 0.4 any)
          consult-locate-args "plocate --ignore-case --regexp")
    (add-to-list 'consult-preview-allowed-hooks
                 'global-org-modern-mode-check-buffers)
    (add-to-list 'consult-preview-allowed-hooks
                 'global-hl-todo-mode-check-buffers)
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file
     consult--source-recent-file consult--source-project-recent-file
     consult--source-bookmark
     :preview-key 'any)
    (when (modulep! :config default)
      (consult-customize
       +default/search-project +default/search-other-project
       +default/search-project-for-symbol-at-point
       +default/search-cwd +default/search-other-cwd
       +default/search-notes-for-symbol-at-point
       +default/search-emacsd
       :preview-key 'any))
    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window))

  (after! helm
    (setq helm-split-window-default-side 'right)))


;;; Term

(when cae-init-term-enabled-p
  (load! "term"))


;;; Text

(when cae-init-text-enabled-p
  (load! "text"))


;;; Email

;; Always enable the essential email configuration, including maildirs and addresses.
;; This is crucial for testing and debugging any issues with the email setup.
(setq user-full-name "StrawberryTea"
      user-mail-address "look@strawberrytea.xyz"
      mail-host-address "strawberrytea.xyz"
      mail-source-directory "~/.mail/")
(make-directory "~/.mail/Fastmail/" t)

(after! mu4e
  (setq mu4e-eldoc-support t)
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Fastmail"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Fastmail"
                                 (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "look@strawberrytea.xyz")
                    (user-full-name . "StrawberryTea")
                    (smtpmail-smtp-server . "smtp.fastmail.com")
                    (smtpmail-default-smtp-server . "smtp.fastmail.com")
                    (smtpmail-stream-type . tls)
                    (smtpmail-smtp-service . 465)
                    (mu4e-trash-folder . "/Fastmail/Trash")
                    (mu4e-refile-folder . "/Fastmail/Archive")
                    (mu4e-drafts-folder . "/Fastmail/Drafts")
                    (mu4e-sent-folder . "/Fastmail/Sent"))))))

(after! message
  (setq message-default-mail-headers "Bcc: look@strawberrytea.xyz\n"))

(after! smtpmail
  (setq smtpmail-smtp-server "smtp.fastmail.com"
        smtpmail-default-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'tls
        smtpmail-queue-mail nil
        smtpmail-queue-dir "~/.mail/queued-mail/"
        smtpmail-servers-requiring-authorization ".*"
        smtpmail-smtp-user user-mail-address))

(setq +notmuch-sync-backend 'mbsync
      +notmuch-mail-folder "~/.mail/fastmail")

(when cae-init-email-enabled-p
  (load! "email"))


;;; Applications

(when cae-init-applications-enabled-p
  (when (modulep! :ui workspaces)
    (add-hook! 'circe-channel-mode-hook
      (defun cae-circe-add-channel-to-workspace-h ()
        (when (+workspace-exists-p +irc--workspace-name)
          (persp-add-buffer (current-buffer)))))
    (defadvice! cae-irc-inhibit-workspace-saving-a (&optional inhibit-workspace)
      :after #'+irc-setup-wconf
      (when (and (modulep! :ui workspaces)
                 (not inhibit-workspace))
        (set-persp-parameter 'dont-save-to-file t (+workspace-get +irc--workspace-name))))))


;;; Languages

(when cae-init-languages-enabled-p

;;;; Logs

  ;; Do not highlight quoted strings in syslog-mode because sometimes they
  ;; aren't balanced, which breaks font-lock.
  (after! syslog-mode
    (setq syslog-font-lock-keywords
          (cl-remove-if
           (lambda (keyword)
             (cl-destructuring-bind (regexp . face) keyword
               (string= "'[^']*'" regexp)))
           syslog-font-lock-keywords)
          ;; I don't use syslog notes.
          syslog-note-thing #'ignore))
  (add-hook 'syslog-mode-hook #'cae-apply-ansi-color-to-buffer-h)
  (advice-add #'syslog-load-notes :override #'ignore)
  (add-to-list 'auto-mode-alist '("/var/log.*\\'" . syslog-mode))
  (add-to-list 'auto-mode-alist '("\\.log\\'" . syslog-mode))

;;;; C/C++

  (add-hook 'c-mode-common-hook #'subword-mode)

;;;; Fennel

  (add-hook 'fennel-mode-hook #'outline-minor-mode)

;;;; Python

  (after! nose
    (when (executable-find "nose2")
      (setq nose-global-name "nose2")))
  (when (modulep! :lang python +pyright)
    (after! lsp-pyright
      (setq lsp-pyright-langserver-command "basedpyright")))

;;;; Idris

  (after! idris-settings
    (when (executable-find "idris2")
      (setq idris-interpreter-path "idris2")))

;;;; Lua

  (add-hook 'lua-mode-hook #'subword-mode)
  (add-hook 'lua-mode-hook #'outline-minor-mode)
  (setq-hook! 'lua-mode-hook
    outline-regexp "[ 	]*---\\(-*\\**\\) [^ 	\n]")

  ;; Prevent heading backgrounds from being overwritten.
  (when (bound-and-true-p cae-theme-extend-heading-faces)
    (after! lsp
      (add-hook! 'lua-mode-hook
        (defun cae-lsp-do-not-highlight-comments-h ()
          (setq-local lsp-semantic-token-faces
                      (assoc-delete-all "comment" lsp-semantic-token-faces))))))

;;;; Lean 4

  (setq-hook! 'lean4-mode-hook
    tab-width 2)

;;;; Haskell

  (use-package! consult-hoogle
    :when (modulep! :lang haskell)
    :defer t :init
    (after! haskell-mode
      (map! :map haskell-mode-map
            :localleader
            "g" #'consult-hoogle
            "G" #'consult-hoogle-project))))

;;; Appendix

(when cae-init-appendix-enabled-p
  (doom-load-packages-incrementally
   `(,@(when (modulep! :completion corfu)
         '(corfu))
     ,@(when (modulep! :emacs dirvish)
         '(dired transient dirvish))
     bookmark auth-source tramp-compat tramp-integration tramp tramp-sh
     ,@(when (modulep! :term eshell)
         '(esh-util esh-module esh-proc esh-io esh-cmd eshell
           em-tramp em-smart em-banner em-basic em-cmpl
           em-extpipe em-glob em-hist em-ls em-script em-term
           em-alias em-elecslash em-rebind em-prompt))
     ,@(when (and (modulep! :term vterm)
                  (executable-find "cmake"))
         '(vterm))
     ,@(when (modulep! :ui hydra)
         '(hydra))
     ,@(when (modulep! :email mu4e)
         '(mu4e))
     ,@(when (or (modulep! :completion helm)
                 (modulep! :cae helm))
         '(async helm-lib helm-multi-match helm-source helm-core
           helm-global-bindings helm))
     ,@(when (modulep! :app rss)
         '(elfeed))
     ,@(when (modulep! :cae misc-applications) ; Music apps
         (nconc '(emms elfeed-tube empv somafm helm-emms lyrics-fetcher)
                (when (executable-find "mpd")
                  '(mpc))))
     ,@(when (modulep! :cae ai)
         '(copilot whisper greader org-ai chatgpt-shell gptel magit-gptcommit))
     ,@(when (modulep! :tools direnv)
         '(envrc))
     ,@(when (and (modulep! :tools lsp)
                  (not (modulep! :tools lsp +eglot)))
         '(lsp-mode lsp-ui))
     ,@(when (and (modulep! :tools lsp)
                  (not (modulep! :tools lsp +eglot))
                  (modulep! :lang cc))
         '(lsp-clangd))
     ,@(when (modulep! :tools lsp +eglot)
         '(eglot))
     ,@'(eww)
     ,@(when (modulep! :tools magit)
         '(parrot))
     ,@'(parrot-rotate)
     ,@(when (modulep! :cae gnus)
         '(gnus gnus-group gnus-sum gnus-srvr))
     ,@(when (modulep! :tools pdf)
         '(image-mode pdf-util pdf-info pdf-cache pdf-view pdf-tools))
     ,@(when (modulep! :ui treemacs)
         '(treemacs))
     ,@(when (and (modulep! :ui treemacs +lsp)
                  (not (modulep! :tools lsp +eglot)))
         '(treemacs-lsp))
     ,@(when (modulep! :app rss +org)
         '(elfeed-org))
     ,@(when (and (modulep! :completion vertico)
                  (modulep! :cae misc-applications))
         '(consult-gh))
     ,@(when (and (modulep! :completion vertico)
                  (modulep! :email mu4e)
                  (executable-find "mu"))
         '(consult-mu))
     ,@(when (and (or (modulep! :completion helm)
                      (modulep! :cae helm))
                  (modulep! :cae misc-applications))
         '(helm-system-packages helm-emms helm-linux-disks helm-rage))
     ,@(when (modulep! :cae misc-applications)
         (nconc                         ; The music apps are in a previous line.
          ;; System
          '(trashed pulseaudio-control disk-usage daemons neato-graph-bar
            journalctl-mode)
          ;; Games
          '(snake speed-type tetris bubbles dunnet autotetris klondike)
          ;; Eye candy
          '(fireplace flames-of-freedom snow zone zone-matrix zone-rainbow
            zone-nyan zone-pgm-spoopy selectric)
          ;; Insert
          '(lorem-ipsum password-generator uuidgen)
          ;; Random
          '(pomm debbugs pomm noaa hackernews leetcode)))
     ,@(when (modulep! :editor multiple-cursors)
         (if (modulep! :editor evil)
             '(evil-multiedit evil-mc)
           '(multiple-cursors)))
     ,@(when (modulep! :editor format)
         '(apheleia)))
   t)

  (unless (or (executable-find "termux-setup-storage")
              (not (cae-display-graphic-p)))
    (after! pdf-tools
      (pdf-tools-install t nil t)))

  (after! copilot
    (unless (or (not copilot-node-executable)
                (file-exists-p copilot-install-dir))
      (copilot-install-server)))

  ;; Do not spam me with warnings.
  (unless init-file-debug
    (setq warning-minimum-level :error
          warning-minimum-log-level :error)))

(setq cae-config-finished-loading t)

;;Local Variables:
;;eval: (when (featurep 'aggressive-indent) (aggressive-indent-mode -1))
;;End:
