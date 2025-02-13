;;; UI configuration

(when cae-init-ui-enabled-p
  (load! "lisp/cae-theme")
  (load! "lisp/cae-visual-scrolling")
  ;; Show absolute line numbers. I prefer to not show relative line numbers
  ;; because I use `avy' commands to jump to lines.
  (setq display-line-numbers-type nil
        display-line-numbers-width-start t)
  (autoload 'minibuffer-depth-setup "mb-depth")
  (add-hook 'minibuffer-setup-hook  #'minibuffer-depth-setup)
  (add-hook 'tabulated-list-mode-hook #'hl-line-mode)
  (after! tab-bar
    (setq tab-bar-show 1))
  (setq confirm-kill-processes nil)
  (setq x-stretch-cursor t
        kill-buffer-delete-auto-save-files t
        window-combination-resize t
        scroll-preserve-screen-position t
        suggest-key-bindings nil)
  (after! persp-mode
    (setq persp-reset-windows-on-nil-window-conf t))
  (after! image
    (setq image-use-external-converter t))
  (setq set-message-functions
        '(inhibit-message
          set-minibuffer-message)
        inhibit-message-regexps '("C-g is undefined\\'"
                                  "ESC is undefined\\'"))
  (after! flymake
    (setq flymake-start-on-flymake-mode nil))
  (setq-hook! '(prog-mode-hook conf-mode-hook text-mode-hook)
    scroll-margin 2)
  (after! time
    (setq display-time-default-load-average nil))
  (after! bind-key
    (setq bind-key-describe-special-forms t))
  (after! transient
    (setq transient-align-variable-pitch t))
  (after! newcomment
    (setq comment-empty-lines 'eol
          comment-padding nil))
  (after! doom-modeline
    (setq doom-modeline-hud t
          doom-modeline-support-imenu t
          doom-modeline-mu4e t
          doom-modeline-gnus t
          doom-modeline-github t
          doom-modeline-major-mode-icon nil
          doom-modeline-minor-modes nil))
  (after! which-key
    (setq which-key-ellipsis "..."
          which-key-idle-delay 0.5
          which-key-compute-remaps t
          which-key-max-description-length 35
          which-key-preserve-window-configuration t
          which-key-show-transient-maps nil))
  (when (modulep! :editor evil)
    (after! evil
      (add-to-list 'evil-buffer-regexps `(,(concat "\\`" (regexp-quote " *which-key*") "\\'")))))
  (when (modulep! :completion vertico)
    (after! which-key
      (setq which-key-use-C-h-commands t))
    (defvar cae-which-key-current-keymap nil)
    (defadvice! cae-which-key-update-current-keymap-a
      (_keymap-name keymap &rest args)
      :before #'which-key--show-keymap
      (setq cae-which-key-current-keymap keymap))
    (defadvice! cae-which-key-consult-C-h-dispatch (oldfun)
      :around #'which-key-C-h-dispatch
      (cond ((not (which-key--popup-showing-p))
             (setq this-command 'embark-prefix-help-command)
             (call-interactively #'embark-prefix-help-command))
            ((string-empty-p (which-key--current-key-string))
             (setq this-command 'embark-prefix-help-command)
             (embark-bindings-in-keymap cae-which-key-current-keymap))
            (t (call-interactively #'embark-prefix-help-command)))))
  (setq +zen-text-scale 0)
  (after! mule-util
    (setq truncate-string-ellipsis "..."))
  (after! alert
    (setq alert-default-style 'libnotify))
  (when (modulep! :ui window-select +numbers)
    (after! winum
      (setq winum-auto-setup-mode-line t))
    (add-hook 'doom-after-init-hook #'winum-mode)
    (remove-hook 'doom-switch-window-hook #'winum-mode))
  (after! shr
    (setq shr-width 120
          shr-max-width 120
          shr-use-xwidgets-for-media (featurep 'xwidget-internal))
    (setq shr-use-fonts nil)
    (advice-add #'shr-colorize-region :around #'ignore))
  (add-hook 'compilation-mode-hook #'doom-mark-buffer-as-real-h)
  (when (modulep! :completion vertico)
    (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target))
  (after! eldoc
    (setq eldoc-echo-area-prefer-doc-buffer t
          eldoc-display-functions (delq 'eldoc-display-in-buffer eldoc-display-functions)))
  (use-package! info-colors
    :defer t :init (add-hook 'Info-selection-hook #'info-colors-fontify-node))
  (use-package! communinfo
    :after info :config
    (setopt Info-url-alist communinfo-url-alist))
  (use-package! authinfo-color-mode
    :defer t :init
    (add-to-list 'auto-mode-alist '("authinfo.gpg\\'" . authinfo-color-mode))
    (add-to-list 'auto-mode-alist '("authinfo\\'" . authinfo-color-mode))
    (advice-add 'authinfo-mode :override #'authinfo-color-mode))
  (when (modulep! :ui popup)
    (set-popup-rules!
      '(("\\`\\*Backtrace\\*" :size +popup-shrink-to-fit :quit nil :ttl nil :vslot 99)
        ("\\`\\*exwm" :ignore t)
        ("\\`\\*difftastic git diff\\*\\'" :size +popup-shrink-to-fit :select t :quit t :side bottom :ttl 0)
        ("\\`\\*Pp Eval Output\\*" :size +popup-shrink-to-fit :quit t :ttl t)
        ("\\`\\*org-roam\\*" :size 60 :side left :select nil :quit nil)
        ("\\`\\*info.*" :size cae-popup-resize-help-buffer :side right :ttl nil :select t :quit t :ttl t :slot 2 :vslot 2)
        ("\\`\\*\\(?:Wo\\)?Man " :size cae-popup-resize-help-buffer :side right :ttl t :select t :quit t :ttl 0 :vslot -6)
        ("\\`\\*tldr\\*" :size cae-popup-resize-help-buffer :side right :select t :quit t)
        ("\\`\\*Diff\\*" :size cae-popup-resize-help-buffer :side right :select t :quit t :ttl 0)
        ("\\`\\*Ibuffer Diff\\*" :size cae-popup-resize-help-buffer :side right :select t :quit t :ttl 0)
        ("\\`\\*\\([Hh]elp\\|Apropos\\)" :size cae-popup-resize-help-buffer :side right :select t :quit t :ttl 0 :slot 2 :vslot -8)
        ("\\` \\*Metahelp.*" :size cae-popup-resize-help-buffer :side right :select t :quit t :ttl 0 :slot 2 :vslot -9)
        ("\\`\\*Messages\\*" :vslot -10 :height 10 :side bottom :select t :quit t :ttl nil :vslot 99)
        ("\\`\\*eww.*" :size cae-popup-resize-help-buffer :side right :select t :ttl nil)
        ("\\`\\*w3m\\*\\'" :size cae-popup-resize-help-buffer :side right :select t :ttl nil)
        ("\\`\\*dap-ui-repl\\*\\'" :vslot -5 :size 0.3 :select t :modeline nil :quit nil :ttl nil)
        ("\\`SpeedRect Command Key Help\\'" :size cae-popup-resize-help-buffer :side right :select nil :quit t :ttl 0)
        ("\\`\\*ednc-log\\*\\'" :size cae-popup-resize-help-buffer :side right :select nil :quit t :ttl nil)
        ("*Notification [0-9]+" :side top :size +popup-shrink-to-fit :select nil)
        ("\\`\\*tldr\\*\\'" :size cae-popup-resize-help-buffer :side right :ttl t :select t :quit t :ttl 0)
        ("\\`\\*Shortdoc .*" :size cae-popup-resize-help-buffer :side right :ttl t :select t :quit t :ttl 0)
        ("\\`\\*devdocs\\*\\'" :width 122 :side right :ttl t :select t :quit t :ttl 0)
        ("\\`Trash Can" :size 0.3 :side bottom :select t :quit t :ttl 0)
        ("\\`\\*evil-owl\\*\\'" :side bottom :select nil :ttl 0 :size cae-popup-shrink-to-fit)
        ("\\`\\*chatgpt\\* " :size 0.3 :select t :quit nil :ttl nil)
        ("\\`\\*dall-e\\*.*" :size 0.3 :select t :quit nil :ttl nil)
        ("\\`\\*edit-indirect " :side top :select t :ttl 0 :size cae-popup-shrink-to-fit)
        ("\\`\\*vterm" :quit nil :ttl nil :size 0.3)
        ("\\`\\*eldoc\\*\\'" :quit t :size +popup-shrink-to-fit :ttl nil :side bottom)
        ("\\`\\*notmuch-hello" :ignore t)
        ("\\`\\*hackernews .*\\*'" :ignore t)
        ("\\`\\*gud-" :ttl nil :size 0.35)
        ("\\`\\*gptel-diff\\*" :ignore t)
        ("\\`\\*gptel-rewrite\\*" :ignore t)
        ("embrace-help" :side top :size +popup-shrink-to-fit)
        ("*helm " :ignore t)
        ("\\`\\*Async Shell Command\\*\\'" :side top :select nil :ttl 0 :quit t :size cae-popup-shrink-to-fit)
        ("*Neato Graph Bar" :side top :quit t :ttl 0 :size (lambda (win) (set-window-text-height win (+ (num-processors) 2))))))
