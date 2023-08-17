;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defvar cae-config-finished-loading nil)

;;; Stuff that should not be disabled.

(load! "lisp/cae-bindings")
(if (modulep! :editor evil)
    (load! "lisp/cae-evil")
  (load! "lisp/cae-holy"))
(load! "lisp/cae-multi")                ;Run parallel Emacs instances.
(load! "lisp/cae-keyboard")             ;Input hacks.
(load! "lisp/cae-smartparens")          ;Allow Smartparens to be disabled. This
                                        ;is also our Smartparens configuration.

;; Helm is not our main completion system.
(when (and (modulep! :completion helm)
           (modulep! :completion vertico))
  (remove-hook 'doom-first-input-hook #'helm-mode))

;; Extra stuff for when we run Doom without any modules loaded.
(unless (or (modulep! :completion helm)
            (modulep! :completion ivy)
            (modulep! :completion vertico))
  (icomplete-mode +1)
  (icomplete-vertical-mode +1))
(unless (modulep! :lang emacs-lisp)
  (remove-hook 'emacs-lisp-mode-hook #'overseer-enable-mode))

;; Stuff so that Emacs doesn't break in the Terminal.
(when (modulep! :completion vertico +childframe)
  (unless (cae-display-graphic-p)
    (remove-hook 'vertico-mode-hook #'vertico-posframe-mode)))
(when (modulep! :ui ligatures)
  (unless (cae-display-graphic-p)
    (setq +ligatures-in-modes nil)
    (remove-hook 'doom-init-ui-hook #'+ligatures-init-h)
    (remove-hook 'doom-init-ui-hook #'+ligature-init-composition-table-h)
    (remove-hook 'doom-init-ui-hook #'+ligatures-init-buffer-h)))

(after! xclip
  (cond ((executable-find "termux-setup-storage")
         (setq xclip-method 'termux-clipboard-get))))

;; For some reason Persp is picking up a few buffers that it should not.
(when (modulep! :ui workspaces)
  (after! persp-mode
    (add-to-list 'persp-add-buffer-on-after-change-major-mode-filter-functions
                 (cae-defun cae-persp-skip-buffer-p (buffer)
                   (string= (buffer-name buffer) "*lsp-log*")))))

;; Set up fonts
(unless (memq system-type '(cygwin windows-nt ms-dos))
  ;; Previously I used Iosevka Comfy and size 18.
  (setq doom-font (font-spec :family "Iosevka Comfy" :size 19)
        doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo"
                                            :size 19)
        doom-unicode-font (unless (modulep! :ui unicode)
                            (font-spec :family "LXGW WenKai" :weight 'light
                                       :size 18))))

;; I have disabled `rainbow-delimiters' as a package so this is to prevent
;; errors.
(defalias 'rainbow-delimiters-mode #'ignore)

;; Do not break my clipboard in SSH sessions.
(when (and (modulep! :os tty)
           (getenv "SSH_TTY")
           (not (cae-display-graphic-p)))
  (remove-hook 'tty-setup-hook #'doom-init-clipboard-in-tty-emacs-h))


;;; UI

(when cae-init-ui-enabled-p
  (load! "lisp/cae-theme")
  (load! "lisp/cae-hydra")

  ;; Show absolute line numbers. I prefer to not show relative line numbers
  ;; because I use `avy' commands to jump to lines.
  (setq display-line-numbers-type (if (modulep! :editor evil) 'relative t)
        display-line-numbers-width-start t)

  ;; Show minibuffer recursion depth
  (autoload 'minibuffer-depth-setup "mb-depth")
  (add-hook 'minibuffer-setup-hook  #'minibuffer-depth-setup)

  ;; I never use the mouse buttons on the tab bar so I prefer to not show them.
  (defadvice! cae-tab-bar-load-buttons-a ()
    :override #'tab-bar--load-buttons
    (setq tab-bar-close-button   nil
          tab-bar-back-button    nil
          tab-bar-forward-button nil
          tab-bar-new-button     nil))
  ;; For some reason `tab-bar' commands sometimes misbehave when they are called
  ;; before `tab-bar-mode' is enabled. I took the easy solution and just enable
  ;; `tab-bar-mode' after a delay.
  (run-with-idle-timer 3 nil #'tab-bar-mode +1)

  ;; Don't confirm when exiting Emacs that active processes exist.
  (setq confirm-kill-processes nil)

  (after! ansi-color
    ;; I am trying this out. Not sure if I like it yet.
    (setq ansi-color-bold-is-bright t))

  (setq x-stretch-cursor t              ;Show me if I am on a TAB or a space
        kill-buffer-delete-auto-save-files t
        scroll-conservatively 0         ;Doom disables this option as a
                                        ;performance optimization but I would
                                        ;much rather have Emacs automatically
                                        ;recenter my windows .
        window-combination-resize t     ;Take new window space from all other
                                        ;windows (not just current)
        scroll-preserve-screen-position 'always)

  (setq set-message-functions
        '(inhibit-message
          set-minibuffer-message)
        inhibit-message-regexps '("C-g is undefined$"
                                  "ESC is undefined$"))

  (setq-hook! '(prog-mode-hook conf-mode-hook text-mode-hook)
    scroll-margin 2)

  (after! time
    (setq display-time-default-load-average nil))

  (after! bind-key
    (setq bind-key-describe-special-forms t))

  (after! transient
    (setq transient-align-variable-pitch t))

  (after! newcomment
    (setq comment-empty-lines 'eol      ;I prefer to comment blank lines with
                                        ;`comment-region' so that I can mark the
                                        ;entire commented text with
                                        ;`mark-paragraph'.
          comment-padding nil))         ;I prefer no spaces between comment
                                        ;delimiters and the comment text.

  (when (and (modulep! :ui modeline)
             (not (modulep! :ui modeline +light)))
    (after! doom-modeline
      (setq doom-modeline-hud t
            doom-modeline-support-imenu t)))

  (after! which-key
    (setq which-key-ellipsis "..."
          which-key-compute-remaps t
          which-key-max-description-length 35
          which-key-separator (if (cae-display-graphic-p) " → " " -> ")))

  (when (cae-display-graphic-p)
    (after! eros
      (setq eros-eval-result-prefix (if (cae-display-graphic-p) "⟹ " "=> ")))) ;Pretty arrow

  (after! mule-util
    (setq truncate-string-ellipsis "...")) ;The unicode ellipsis is ugly to me

  (after! ffap
    ;; Do not count angle brackets as part of file names because then they get
    ;; mixed up with the tags.
    (setf (alist-get 'nxml-mode ffap-string-at-point-mode-alist)
          (list "--:\\\\${}+@-Z_[:alpha:]~*?#" "" "")))

  ;; Do not spam me with warnings on startup.
  (after! warnings
    (setq warning-minimum-level :emergency
          warning-minimum-log-level :emergency)
    (add-hook 'doom-first-file-hook
              (cae-defun cae-warnings-initialize ()
                (setq warning-minimum-level :warning
                      warning-minimum-log-level :warning))
              :append))

  (after! shr
    ;; `shr' wraps lines in a visually unappealing way.
    (setq shr-width 120
          shr-max-width 120)

    ;; I prefer to not use fonts in `shr' because it looks weird with the font setup I have.
    (setq shr-use-fonts nil)

    ;; Sometimes EWW makes web pages unreadable by adding a bright background. Do
    ;; not colorize backgrounds at all.
    (advice-add #'shr-colorize-region :around #'ignore))

  (after! proced
    (setq-default proced-auto-update-flag t))

  ;; Allow switching to these buffers with `C-x b'
  (add-hook 'compilation-mode-hook #'doom-mark-buffer-as-real-h)
  ;;(add-hook 'debugger-mode-hook #'doom-mark-buffer-as-real-h)

  (use-package! info-colors
    :defer t :init (add-hook 'Info-selection-hook #'info-colors-fontify-node))

  (use-package! authinfo-color-mode
    :defer t :init
    (add-to-list 'auto-mode-alist '("authinfo.gpg\\'" . authinfo-color-mode))
    (add-to-list 'auto-mode-alist '("authinfo\\'" . authinfo-color-mode))
    (advice-add 'authinfo-mode :override #'authinfo-color-mode))

  (when (modulep! :ui workspaces)
    (advice-add #'which-key--process-page :around
                #'cae-ui-which-key-show-workspace-a))

  ;; Set some popup rules. How does slot/vslot work? I prefer to set these popup
  ;; rules here instead of in the relevant `use-package!' blocks.
  (when (modulep! :ui popup)
    (set-popup-rules!
      ;; TODO Set the correct slot/vslot for these popups.
      '(("^\\*Backtrace\\*" :size +popup-shrink-to-fit :quit nil
         :ttl nil :vslot 99)
        ("^\\*exwm" :size +popup-shrink-to-fit :ttl nil
         :ttl nil)
        ("^\\*Pp Eval Output\\*" :size +popup-shrink-to-fit
         :quit t :ttl t)
        ("^\\*org-roam\\*" :size 60 :side left :select nil
         :quit nil)
        ("^\\*info.*" :size cae-popup-resize-help-buffer
         :side right :ttl t :select t :quit t :ttl t :slot 2 :vslot 2)
        ("^\\*\\(?:Wo\\)?Man " :size cae-popup-resize-help-buffer
         :side right :ttl t :select t :quit t :ttl 0 :vslot -6)
        ("^\\*tldr\\*" :size cae-popup-resize-help-buffer
         :side right :select t :quit t)
        ("^\\*Diff\\*" :size cae-popup-resize-help-buffer
         :side right :select t :quit t :ttl 0)
        ("^\\*Ibuffer Diff\\*" :size cae-popup-resize-help-buffer
         :side right :select t :quit t :ttl 0)
        ("^\\*\\([Hh]elp\\|Apropos\\)"
         :size cae-popup-resize-help-buffer :side right :select t :quit t :ttl 0
         :slot 2 :vslot -8)
        ("^ \\*Metahelp.*" :size cae-popup-resize-help-buffer
         :side right :select t :quit t :ttl 0 :slot 2 :vslot -9)
        ("^\\*Messages\\*" :vslot -10 :height 10 :side bottom
         :select t :quit t :ttl nil :vslot 99)
        ("^\\*eww.*" :size cae-popup-resize-help-buffer :side right
         :select t :ttl nil)
        ("^\\*w3m\\*$" :size cae-popup-resize-help-buffer
         :side right :select t :ttl nil)
        ("^\\*dap-ui-repl\\*$" :vslot -5 :size 0.3 :select t
         :modeline nil :quit nil :ttl nil)
        ("^SpeedRect Command Key Help$" :size cae-popup-resize-help-buffer
         :side right :select nil :quit t :ttl 0)
        ("^\\*ednc-log\\*$" :size cae-popup-resize-help-buffer
         :side right :select nil :quit t :ttl nil)
        ("*Notification [0-9]+" :side top :size +popup-shrink-to-fit
         :select nil)
        ("^\\*tldr\\*$" :size cae-popup-resize-help-buffer
         :side right :ttl t :select t :quit t :ttl 0)
        ("^\\*Shortdoc .*" :size cae-popup-resize-help-buffer
         :side right :ttl t :select t :quit t :ttl 0)
        ("^\\*devdocs\\*$" :width 122
         :side right :ttl t :select t :quit t :ttl 0)
        ("^Trash Can" :size 0.3 :side bottom :select t :quit t
         :ttl 0)
        ("^\\*chatgpt\\* " :size 0.3 :select t :quit nil :ttl nil)
        ("^\\*vterm" :quit nil :ttl nil :size 0.3)
        ("^\\*notmuch-hello"  :ignore)
        ("^\\*gud-" :ttl nil :size 0.35)))
    (after! embark
      (set-popup-rule! (regexp-quote embark--verbose-indicator-buffer)
        :size #'+popup-shrink-to-fit :side 'bottom :ttl t))
    (after! elfeed
      (set-popup-rule! (format "^%s$" (regexp-quote elfeed-log-buffer-name))
        :size 0.3 :side 'right :select nil :quit t :ttl nil))
    (map! :map messages-buffer-mode-map :n "q" #'quit-window))

  ;; Lower the default popup delay.
  (after! tooltip
    (setq tooltip-hide-delay 3))

  (when (modulep! :checkers syntax +childframe)
    (after! flycheck-posframe
      (setq flycheck-posframe-border-width 1
            flycheck-posframe-border-use-error-face t)))

  ;; Show the window number in the modeline (when applicable).
  (after! winum
    (setq winum-auto-setup-mode-line t))

  ;; Fixes an issue for me where the Vertico posframe would flicker and go blank.
  (when (modulep! :completion vertico +childframe)
    (after! vertico-posframe
      (setq vertico-posframe-parameters
            '((inhibit-double-buffering . t)))))
  (after! posframe
    (setq posframe-inhibit-double-buffering t))

  (use-package! topsy
    :defer t :init (add-hook 'prog-mode-hook #'topsy-mode)
    :config
    ;; Set custom function for rjsx-mode
    ;; Disable topsy-mode for gptel-mode
    (setf (alist-get 'rjsx-mode topsy-mode-functions) #'cae-ui-topsy-rjsx-fn)
    (add-hook 'gptel-mode-hook
              (cae-defun cae-disable-topsy-in-gptel-h ()
                "Disable topsy-mode in `gptel-mode'." ;`gptel' is Karthink's
                                        ;package.
                (topsy-mode -1))))

  (use-package! iscroll
    :defer t :init
    (add-hook 'org-mode-hook #'iscroll-mode)
    (add-hook 'markdown-mode-hook #'iscroll-mode)
    (add-hook 'image-mode-hook #'iscroll-mode)
    (add-hook 'eww-mode-hook #'iscroll-mode)
    (add-hook 'w3m-mode-hook #'iscroll-mode))

  (use-package! beacon
    :defer t :init (add-hook 'doom-first-file-hook #'beacon-mode)
    :config
    (setq beacon-blink-delay 0.15
          beacon-blink-duration 0.15)
    (beacon-mode +1)
    (add-to-list 'beacon-dont-blink-commands 'doom/escape)
    (add-hook 'persp-activated-functions
              (cae-defun cae-beacon-blink-on-workspace-switch (&rest _)
                (run-at-time 0.01 nil #'beacon-blink-automated)))
    (add-hook 'persp-created-functions #'cae-beacon-blink-on-workspace-switch)
    (after! corfu
      (add-hook 'beacon-dont-blink-predicates #'cae-corfu-visible-p))
    (setq beacon-blink-when-window-scrolls nil
          beacon-overlay-priority -1))

  (use-package! anzu
    :defer t :init
    (global-set-key [remap query-replace] 'anzu-query-replace)
    (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
    (define-key isearch-mode-map [remap isearch-query-replace] #'anzu-isearch-query-replace)
    (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
    :config
    (setq anzu-mode-lighter ""
          anzu-replace-threshold 50
          anzu-replace-to-string-separator (if (cae-display-graphic-p) " → " " -> ")))

  (use-package! isearch-mb
    :after-call isearch-mode-hook
    :config
    (isearch-mb--setup)
    (isearch-mb-mode +1)
    (dolist (cmd '(recenter-top-bottom reposition-window
                   scroll-right scroll-left isearch-yank-word
                   consult-isearch-history))
      (add-to-list 'isearch-mb--with-buffer cmd))
    (dolist (cmd '(anzu-isearch-query-replace anzu-isearch-query-replace-regexp
                   avy-isearch consult-line))
      (add-to-list 'isearch-mb--after-exit cmd))
    (define-key isearch-mb-minibuffer-map (kbd "C-w") #'isearch-yank-word)
    (define-key isearch-mb-minibuffer-map (kbd "M-j") #'avy-isearch)
    (when (modulep! :completion vertico)
      (map! :map isearch-mb-minibuffer-map
            [remap consult-history] #'consult-isearch-history)
      (define-key isearch-mb-minibuffer-map (kbd "M-s l") 'consult-line))
    (define-key isearch-mb-minibuffer-map (kbd "M-%")   #'anzu-isearch-query-replace)
    (define-key isearch-mb-minibuffer-map (kbd "M-s %") #'anzu-isearch-query-replace-regexp))

  (use-package! hercules
    :defer t :init
    (after! debug
      (map! :map debugger-mode-map
            "<f6>" #'cae-debugger-cheatsheet))
    (after! edebug
      (map! :map edebug-mode-map
            "<f6>" #'cae-edebug-cheatsheet))
    (after! macrostep
      (map! :map macrostep-mode-keymap
            "<f6>" #'cae-macrostep-cheatsheet))
    :config
    (add-hook 'cae-tab-bar-before-switch-hook #'hercules--hide))

  (use-package! outline-minor-faces
    :defer t :init
    (add-hook 'outline-minor-mode-hook #'outline-minor-faces-mode)))


;;; Tools

(when cae-init-tools-enabled-p
  (load! "lisp/cae-projectile")

  (use-package! w3m
    :defer t :config
    (setq w3m-user-agent
          (string-join
           '("Mozilla/5.0"
             "(Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40)"
             "AppleWebKit/533.1""(KHTML, like Gecko)" "Version/4.0"
             "Mobile Safari/533.")
           " ")
          w3m-command-arguments '("-cookie" "-F"))
    (after! w3m-search
      (setq w3m-search-default-engine "duckduckgo"))
    (map! :map w3m-mode-map
          "o" #'ace-link-w3m))

  ;; Set up the default browser.
  (after! browse-url
    (cond ((getenv "WSL_DISTRO_NAME")
           (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
                 browse-url-generic-args '("/c" "start")))
          ((executable-find "chromium-bin-browser")
           (setq browse-url-generic-program (executable-find "chromium-bin-browser")
                 browse-url-generic-args (when (eq (user-uid) 0)
                                           '("--no-sandbox")))))
    (setq browse-url-browser-function
          (cond ((executable-find "termux-setup-storage")
                 #'browse-url-xdg-open)
                (t #'browse-url-generic))))

  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-to-list 'doom-large-file-excluded-modes 'nov-mode)

  (add-to-list 'auto-mode-alist '("/var/log.*\\'" . syslog-mode))
  (add-to-list 'auto-mode-alist '("\\.log$" . syslog-mode))
  ;; Do not highlight quoted strings in syslog-mode because sometimes they aren't
  ;; balanced, which breaks font-lock.
  (after! syslog-mode
    (setq syslog-font-lock-keywords
          (cl-remove-if
           (lambda (keyword)
             (cl-destructuring-bind (regexp . face) keyword
               (string= "'[^']*'" regexp)))
           syslog-font-lock-keywords)))

  ;; Set up printers.
  (after! lpr (setq printer-name "Brother_HL-2350DW"))
  (after! ps-print (setq ps-printer-name "Brother_HL-2350DW"))

  (setq delete-by-moving-to-trash t
        remote-file-name-inhibit-delete-by-moving-to-trash t
        history-length (expt 2 16)
        make-cursor-line-fully-visible nil ;I forgot why I set this.
        yank-pop-change-selection t)

  (add-hook 'bookmark-bmenu-mode-hook #'cae-bookmark-extra-keywords)
  (after! bookmark
    (setq bookmark-bmenu-file-column 50
          bookmark-watch-bookmark-file nil))

  (after! auth-source
    (setq auth-source-cache-expiry nil
          auth-sources (list (concat doom-user-dir "secrets/authinfo"))
          auth-source-gpg-encrypt-to nil))

  (after! password-cache
    (setq password-cache-expiry nil))

  (after! compile
    ;; Some projects I work on have many warnings I am not interested in and the
    ;; `first-error' value for `compilation-scroll-output' stops scrolling at the
    ;; first warning. It would be nice if it scrolled to the first error instead.
    ;; Since it doesn't though, I just set it to `t' and scroll up manually if
    ;; there are errors.
    (setq compilation-scroll-output t))

  (after! tramp
    (setq tramp-shell-prompt-pattern
          "\\(?:^\\|\r\\)[^]#$%>➜\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"
          tramp-use-scp-direct-remote-copying t)
    (dolist (path '("~/.guix-profile/bin" "~/.guix-profile/sbin"
                    "/run/current-system/profile/bin"
                    "/run/current-system/profile/sbin"))
      (add-to-list 'tramp-remote-path path)))

  ;; Use Emacs as the default editor for shell commands.
  ;; `dwim-shell-command'.
  (dolist (hook '(shell-mode-hook eshell-mode-hook vterm-mode-hook))
    (dolist (fn '(with-editor-export-editor
                  with-editor-export-hg-editor
                  with-editor-export-git-editor))
      (add-hook hook fn)))
  (advice-add #'with-editor-export-editor :around #'cae-hacks-shut-up-a)

  (when (and (modulep! :checkers spell)
             (not (modulep! :checkers spell +flyspell)))
    (after! spell-fu
      (add-to-list 'spell-fu-faces-exclude 'message-header-other)
      (add-to-list 'spell-fu-faces-exclude 'org-property-value)
      (add-to-list 'spell-fu-faces-exclude 'message-header-to)
      (setq spell-fu-faces-exclude
            (delq 'font-lock-string-face spell-fu-faces-include))))

  (when (modulep! :tools pdf)
    (use-package! pdftotext
      :defer t :init
      (defadvice! +pdf-view-mode-a (oldfun &rest args)
        :around #'pdf-view-mode
        (if (cae-display-graphic-p)
            (apply oldfun args)
          (apply #'pdftotext-mode args)))))

  (when (and (modulep! :tools lsp)
             (not (modulep! :tools lsp +eglot)))
    (after! lsp-mode
      (setq lsp-headerline-breadcrumb-enable nil
            lsp-enable-snippet t
            lsp-enable-text-document-color t
            lsp-enable-folding t
            lsp-enable-indentation nil
            lsp-semantic-tokens-enable t)
      (after! lsp-ui
        (setq lsp-signature-auto-activate t
              lsp-ui-doc-include-signature t
              lsp-ui-doc-header nil))
      (after! lsp-clangd
        (setq lsp-clients-clangd-args
              `(,(format "-j=%d" (max 1 (/ (doom-system-cpus) 2)))
                "--background-index"
                "--clang-tidy"
                "--completion-style=detailed"
                "--header-insertion=never"
                "--header-insertion-decorators=0")))
      (after! lsp-lua
        (setq lsp-lua-runtime-version "LuaJIT"
              lsp-lua-hint-enable t
              lsp-lua-hint-set-type t
              lsp-clients-lua-language-server-bin (executable-find "lua-language-server")
              lsp-clients-lua-lsp-server-install-dir lsp-clients-lua-language-server-bin
              lsp-clients-lua-language-server-main-location "/opt/lua-language-server/main.lua"))
      (add-to-list 'lsp-disabled-clients 'ccls)
      (add-to-list 'lsp-disabled-clients 'mspyls)))

  (when (modulep! :tools lsp +eglot)
    (after! eglot
      (let ((clangd '("clangd" "--background-index" "--clang-tidy"
                      "--completion-style=detailed" "--header-insertion=never"
                      "--header-insertion-decorators=0")))
        (if (assoc '(c++-mode c-mode) eglot-server-programs)
            (setf (cdr (assoc '(c++-mode c-mode) eglot-server-programs)) clangd)
          (setq eglot-server-programs
                (cons (cons '(c++-mode c-mode) clangd)
                      eglot-server-programs))))))

  (when (modulep! :checkers spell)
    (after! spell-fu
      (add-hook 'nxml-mode-hook
                (cae-defun cae-disable-spell-fu-h ()
                  (spell-fu-mode -1)))))

  (use-package! dwim-shell-command
    :defer t :init
    (autoload 'dwim-shell-command "dwim-shell-command" nil t)
    (map! [remap shell-command] #'dwim-shell-command
          (:after dired
           :map dired-mode-map
           [remap dired-do-async-shell-command] #'dwim-shell-command
           [remap dired-do-shell-command] #'dwim-shell-command
           [remap dired-smart-shell-command] #'dwim-shell-command)))

  (use-package! 0x0
    :defer t :init
    (map! "C-x U" #'0x0-dwim)
    (after! embark
      (define-key embark-region-map (kbd "U") '0x0-dwim)))

  (use-package! posimacs-shortdocs
    :after shortdoc)

  (use-package! wakatime-mode
    :defer t :defer-incrementally t
    :config
    (global-wakatime-mode +1)
    (setq wakatime-cli-path
          "~/src/wakatime-cli-linux-amd64")))


;;; Editor

(when cae-init-editor-enabled-p
  (load! "lisp/cae-repeat")
  (load! "lisp/cae-visible-mark")
  (load! "lisp/cae-vlf")
  (load! "lisp/cae-multiple-cursors")
  (load! "lisp/cae-restore-point")

  (autoload 'cae-project-bookmark (concat doom-user-dir
                                          "lisp/cae-project-bookmark"))
  (autoload 'cae-project-bookmark-set (concat doom-user-dir
                                              "lisp/cae-project-bookmark"))
  (autoload 'cae-project--get-bookmark-file (concat doom-user-dir
                                                    "lisp/cae-project-bookmark"))
  (map! :desc "project-bookmark" "C-x r p" #'cae-project-bookmark
        :desc "project-bookmark-set" "C-x r P" #'cae-project-bookmark-set)

  ;; `vimish-fold' persists folds by saving the overlay region `(point) (mark)'.
  ;; This is problematic because it means that a fold can be broken by an
  ;; external file change. Still, I use this for personal Org files and the
  ;; like.
  (when (modulep! :editor fold)
    (add-hook 'doom-first-file-hook #'vimish-fold-global-mode)
    (setq vimish-fold-indication-mode 'right-fringe))

  ;; Ensure local elisp packages are up-to-date.
  (add-hook! 'emacs-lisp-mode-hook
    (add-hook 'after-save-hook #'cae-compile-rebuild-package nil t))

  ;; Enable all disabled commands.
  (setq disabled-command-function nil)

  ;; Treat all themes as safe.
  (setq custom-safe-themes t)

  ;; Sensibly handle the primary selection.
  ;;(setq save-interprogram-paste-before-kill t
  ;;      select-enable-primary t)
  ;;(lost-selection-mode +1)

  (setq delete-active-region t)         ;makes `d' delete region in Meow.

  ;; Allow us to undo deleting frames.
  (undelete-frame-mode +1)

  ;; Automatically deindent items when they are added to the kill ring.
  (add-hook #'doom-first-input-hook #'kill-ring-deindent-mode)

  (add-hook 'c-mode-common-hook #'subword-mode)

  (after! paren
    (setq show-paren-context-when-offscreen 'overlay))

  (advice-add #'doom/kill-this-buffer-in-all-windows :around #'doom-set-jump-a)
  (advice-add #'kill-buffer-and-window :around #'doom-set-jump-a)
  (advice-add #'kill-this-buffer :around #'doom-set-jump-a)

  ;; Query buffers for a diff before killing them.
  (defvar cae-diff-window nil
    "Variable to store the diff window created by 'cae-ask-kill-buffer'.")
  (defun cae-ask-kill-buffer ()
    "Ask to diff, save or kill buffer"
    (if (and (buffer-file-name)
             (buffer-modified-p))
        (prog1 (cl-loop for ch = (read-key "(k)ill buffer, (d)iff buffer, (s)ave buffer, (q)uit?")
                        if (or (eq ch ?k) (eq ch ?K))
                        return t
                        if (or (eq ch ?d) (eq ch ?D))
                        do (setq cae-diff-window (diff-buffer-with-file))
                        if (or (eq ch ?s) (eq ch ?S))
                        return (progn (save-buffer) t)
                        if (memq ch '(?q ?Q))
                        return nil)
          (when cae-diff-window
            (delete-window cae-diff-window)
            (setq cae-diff-window nil)))
      t))
  
  (add-to-list 'kill-buffer-query-functions #'cae-ask-kill-buffer)

  ;; Automatically reindent after commenting.
  (advice-add #'comment-or-uncomment-region :after #'indent-region)

  ;; Allow remembering risky variables.
  (advice-add 'risky-local-variable-p :override #'ignore)

  ;; Kill buffers without asking.
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

  ;; Do not automatically continue comments.
  (advice-remove #'newline-and-indent
                 #'+default--newline-indent-and-continue-comments-a)

  (after! expand-region
    (setq expand-region-smart-cursor t))

  ;; Pop mark multiple times with `C-u C-SPC C-SPC ...'.
  (setq set-mark-command-repeat-pop t)

  (setq search-whitespace-regexp ".*?"
        search-default-mode #'char-fold-to-regexp
        isearch-lax-whitespace t
        isearch-wrap-pause 'no-ding
        isearch-lazy-count t
        isearch-repeat-on-direction-change t
        isearch-allow-motion t
        isearch-allow-scroll t
        isearch-yank-on-move 'shift
        isearch-motion-changes-direction t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil    ; Using the suffix for counting matches
                                        ; is better but does not work with
                                        ; `isearch-mb'.
        lazy-highlight-cleanup nil
        ;; The default search ring size is 16, which is too small considering that
        ;; we can fuzzy search the history with Consult.
        search-ring-max 200
        regexp-search-ring-max 200)
  (add-hook 'doom-escape-hook
            (cae-defun cae-clean-up-lazy-highlight-h ()
              (when isearch-lazy-highlight-overlays
                (lazy-highlight-cleanup t) t)))

  ;; Autokill buffers which have not been displayed for 3 days.
  (run-with-idle-timer 30 nil #'midnight-mode +1)
  (after! midnight
    (setq clean-buffer-list-kill-regexps '("\\`\\*.*\\*\\'")
          clean-buffer-list-delay-special 7200)
    (add-to-list 'clean-buffer-list-kill-never-buffer-names
                 doom-fallback-buffer-name))

  (after! outline
    (setq outline-minor-mode-use-buttons nil))

  (after! ispell
    (setq ispell-quietly t
          ispell-dictionary "en_US"
          ispell-help-in-bufferp 'electric))

  (after! vline
    (setq vline-idle-time 0.1))

  (when (modulep! :emacs undo)
    (after! undo-fu
      (setq undo-fu-allow-undo-in-region t)))

  ;; Hide commands in M-x which do not work in the current mode. Vertico commands
  ;; are hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  (use-package! avy
    :defer t :init
    (advice-add #'avy-goto-end-of-line :around
                (cae-defun cae-avy-use-post-style-a (oldfun &rest args)
                  (let ((avy-style 'post))
                    (apply oldfun args))))
    :config
    (setq avy-timeout-seconds 0.4
          avy-all-windows t
          avy-keys (cae-keyboard-remap
                    '(?a ?s ?d ?f ?g
                      ?h ?j ?k ?l ?\;))
          avy-dispatch-alist
          (cae-keyboard-remap
           '((?z . avy-action-zap-to-char)
             (?x . cae-avy-action-kill)
             (?e . cae-avy-action-embark-act)
             (?1 . cae-avy-parrot-rotate)
             (?! . cae-avy-parrot-rotate-backward)
             (?3 . cae-avy-action-comment-dwim)))
          avy-styles-alist '((avy-isearch . pre)
                             (ace-link-man . pre)
                             (avy-goto-end-of-line . post)
                             (avy-kill-ring-save-region . pre)
                             (avy-kill-region . pre)
                             (avy-copy-region . pre)
                             (avy-move-region . pre))
          avy-column-line-overlay t))

  (use-package! tabgo
    :commands tabgo :defer t
    :config
    (setq tabgo-tab-line-keys (cae-keyboard-kbd tabgo-tab-line-keys)))

  (use-package! zop-to-char
    :defer t :init
    (map! [remap zap-to-char] #'zop-to-char
          [remap zap-up-to-char] #'zop-up-to-char)
    :config
    (setq zop-to-char-kill-keys '(?\C-m ?\C-k ?\C-w)))

  ;; I mostly use this package for some additional prefix argument stuff like
  ;; using `C-u - M-:' to insert a string from Elisp without double quotes.
  (use-package! pp+
    :defer t :init
    (defvaralias 'pp-read-expression-map 'minibuffer-local-map)
    (map! [remap eval-last-sexp] #'cae-eval-last-sexp
          [remap eval-expression] #'cae-eval-expression)
    (when (modulep! :tools eval +overlay)
      (after! eros
        (add-hook 'eros-mode-hook
                  (cae-defun cae-eros-setup-keybindings-h ()
                    (map! [remap eval-last-sexp] #'cae-eval-last-sexp))))))

  (use-package! abbrev
    :defer t :config
    (setq-default abbrev-mode t
                  save-abbrevs 'silently)
    (setq abbrev-suggest t)
    (map! :map edit-abbrevs-mode-map
          [remap save-buffer] #'abbrev-edit-save-buffer)
    (map! :map abbrev-map "e" #'edit-abbrevs)
    (advice-add #'abbrev-edit-save-buffer :after #'edit-abbrevs-redefine))

  (use-package! ibuffer
    :defer t :config
    (setq ibuffer-always-show-last-buffer t
          ibuffer-formats
          '((mark modified read-only locked " "
             (name 23 23 :left :elide)  ;Give more space to the name.
             " "
             (size 9 -1 :right)
             " "
             (mode 16 16 :left :elide)
             " "
             (vc-status 12 :left)
             " " filename-and-process)
            (mark " "
                  (name 16 -1)
                  " " filename)))
    (add-to-list 'ibuffer-never-show-predicates #'doom-unreal-buffer-p))

  (use-package! yank-indent
    :defer t :init (add-hook 'doom-first-buffer-hook #'global-yank-indent-mode)
    :config
    (advice-add #'cae-yank-indent-a :after #'yank-indent--after-yank-advice))

  (use-package! hungry-delete
    :defer t :init (add-hook 'aggressive-indent-mode-hook #'hungry-delete-mode)
    :config
    (when (modulep! :config default +smartparens)
      (map! :map hungry-delete-mode-map
            [remap backward-delete-char-untabify] #'sp-backward-delete-char
            [remap c-electric-backspace] #'sp-backward-delete-char
            [remap c-electric-delete-forward] #'cae-delete-char
            [remap delete-backward-char] #'sp-backward-delete-char
            [remap delete-char] #'cae-delete-char
            [remap delete-forward-char] #'cae-delete-char))
    (add-to-list 'hungry-delete-except-modes 'eshell-mode))

  (use-package! file-info
    :defer t :init
    (map! :leader :desc "Show file info" "fi" #'file-info-show)
    :config
    ;; See the `:private vc' module for further configuration.
    (setq file-info-include-headlines t
          file-info-max-value-length 100))

  (use-package! titlecase
    :defer t :init
    (after! embark
      (define-key embark-region-map "T" #'titlecase-region)
      (define-key embark-heading-map "T" #'titlecase-line)
      (define-key embark-sentence-map "T" #'titlecase-sentence)))

  ;; Type `?' during `rectangle-mark-mode' for a help buffer describing the
  ;; `speedrect' commands.
  (use-package! speedrect
    :after-call rectangle-mark-mode-hook
    :config
    (speedrect-hook))

  ;; Make Emacs's sentence commands work with Mr., Mrs., e.g., etc., without
  ;; `sentence-end-double-space'. This package's settings should be tweaked if you
  ;; use multiple languages.
  (use-package! sentex
    :defer t :init
    (map! [remap kill-sentence] #'sentex-kill-sentence
          [remap forward-sentence] #'sentex-forward-sentence
          [remap backward-sentence] #'sentex-backward-sentence))

  (use-package! string-edit-at-point    ; Used in `cae-edit-indirect-dwim'.
    :defer t)

  (after! outline
    (setq outline-font-lock-faces nil
          outline-font-lock-keywords nil)
    (after! which-key
      (which-key-add-keymap-based-replacements outline-minor-mode-map
        "C-c @" "outline")))

  (use-package! expand-region-improved
    :defer t :init
    ;; Keybindings in `lisp/cae-bindings'.
    :config
    (eri/define-pair org-table-cell "|" 'org-at-table-p)
    (eri/add-mode-expansions 'org-mode
      '((eri/mark-inside-org-table-cell
         eri/mark-outside-org-table-cell)))
    (setq eri/try-expand-list
          '((er/mark-symbol
             er/mark-symbol-with-prefix
             er/mark-next-accessor)
            (er/mark-inside-quotes
             eri/mark-outside-quotes)
            (er/mark-inside-pairs
             er/mark-outside-pairs)
            cae-mark-comment
            er/mark-url
            er/mark-email
            eri/mark-line
            eri/mark-block
            mark-page)))

  (use-package! embark
    :defer t :config
    (map! :map embark-collect-mode-map
          "<f6>" #'cae-embark-collect-cheatsheet-hydra/body)
    (define-key vertico-map (kbd "C-z") 'cae-embark-act-with-completing-read)
    (advice-add #'embark-completing-read-prompter :around
                #'cae-bind-C-z-to-abort-a))

  ;;(use-package! jinx
  ;;  :defer t :init
  ;;  (dolist (hook '(text-mode-hook prog-mode-hook conf-mode-hook))
  ;;    (add-hook hook #'jinx-mode))
  ;;  ;;(advice-add 'jinx--correct-replace :before #'cae-jinx--add-to-abbrev)
  ;;  )

  (use-package! logos
    :defer t :custom
    (logos-outlines-are-pages t)
    :bind
    ([remap forward-page] . logos-forward-page-dwim)
    ([remap backward-page] . logos-backward-page-dwim)
    ([remap narrow-to-page] . cae-narrow-to-page))

  (use-package! indent-bars
    :defer t :hook ((python-mode yaml-mode) . indent-bars-mode)))


;;; Autocompletion

(when cae-init-autocompletion-enabled-p
  (when (modulep! :private corfu)
    (load! "lisp/cae-corfu"))
  (load! "lisp/cae-ido")

  (after! yasnippet
    (setq yas-triggers-in-field t))     ;Allow nested snippets.

  (use-package! dabbrev
    :defer t :config
    (defun cae-dabbrev-friend-buffer (other-buffer)
      (< (buffer-size other-buffer) (* 1 1024 1024)))
    (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")
          dabbrev-upcase-means-case-search t
          dabbrev-friend-buffer-function #'cae-dabbrev-friend-buffer))

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

  (use-package! isearch-dabbrev
    :defer t :init
    (map! :map isearch-mode-map
          "M-/" #'isearch-dabbrev-expand
          "C-M-/" #'isearch-dabbrev-expand))

  (when (modulep! :completion vertico)
    (use-package! consult
      :defer t :init
      ;; See `lisp/cae-bindings' for keybindings.
      :config
      (setq consult-preview-key 'any)
      (consult-customize
       consult-ripgrep consult-git-grep consult-grep
       consult-bookmark consult-recent-file
       +default/search-project +default/search-other-project
       +default/search-project-for-symbol-at-point
       +default/search-cwd +default/search-other-cwd
       +default/search-notes-for-symbol-at-point
       +default/search-emacsd
       consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
       :preview-key 'any)
      ;; Optionally configure the register formatting. This improves the register
      ;; preview for `consult-register', `consult-register-load',
      ;; `consult-register-store' and the Emacs built-ins.
      (setq register-preview-delay 0.5
            register-preview-function #'consult-register-format)

      ;; Optionally tweak the register preview window.
      ;; This adds thin lines, sorting and hides the mode line of the window.
      (advice-add #'register-preview :override #'consult-register-window))))


;;; Term

;; Enable Fish autocompletion in `read-shell-command'.
(advice-add #'shell-completion-vars :after #'fish-completion-mode)

(after! em-glob
  ;; Allow us to type HEAD~1, HEAD~2, etc., as arguments to git commands.
  (setq eshell-error-if-no-glob nil))

(after! em-term
  ;; Some of the commands I copied from other configurations and will likely
  ;; never use.
  (setq eshell-visual-commands
        '("ranger" "vi" "screen" "top" "less" "more" "lynx"
          "ncftp" "pine" "tin" "trn" "elm" "vim" "nmtui" "alsamixer" "htop"
          "elinks" "tail" "nano" "ssh" "python" "tmux" "telnet" "fzf"
          "pulsemixer" "ranger" "bluetoothctl" "watch" "ncmpcpp" "btm"
          "ptpython" "ipython" "pshell" "nmtui" "dstat" "pgcli" "vue" "ngrok")
        eshell-visual-subcommands `(("gh" "repo" "fork")
                                    ("geth" "attach")
                                    ,@(unless (string= (getenv "GIT_PAGER") "cat")
                                        '(("git" "log" "diff" "show"))))
        eshell-visual-options '(("git" "--help" "--paginate"))))


;;; Org

(setq doom-scratch-initial-major-mode 'org-mode)

(after! calendar
  (setq calendar-week-start-day 1))

(after! org
  (setq org-directory "~/org/"
        org-extend-today-until 4
        org-startup-with-inline-images t
        org-image-actual-width t
        org-log-done 'time
        org-log-done-with-time t
        org-ellipsis " ..."
        org-archive-location (concat org-directory ".archive/%s::")
        org-hide-emphasis-markers t
        ;; All my computers use 64-bit processors
        org-read-date-force-compatible-dates nil)
  (when (modulep! :lang org +roam2)
    (setq +org-roam-auto-backlinks-buffer nil))
  (map! :map org-mode-map
        "C-c C-M-h" #'er/mark-org-code-block)

  (after! org-crypt
    (setq org-crypt-disable-auto-save 'encrypt))
  (after! org-agenda
    (setq org-agenda-sticky nil
          org-agenda-files '("~/org/")))
  (when (and (modulep! :ui ligatures)
             (eq (car +ligatures-in-modes) 'not))
    (add-to-list '+ligatures-in-modes 'org-mode t #'eq))

  (after! which-key
    (which-key-add-keymap-based-replacements org-mode-map
      "C-c \"" "plot"
      "C-c C-v" "org-babel-map")))

;;; Email

(setq user-full-name "StrawberryTea"
      user-mail-address "look@strawberrytea.xyz"
      mail-host-address "strawberrytea.xyz")

(autoload 'async-smtpmail-send-it "smtpmail-async" nil t)
(after! sendmail
  (setq send-mail-function #'async-smtpmail-send-it))
(after! message
  (setq message-send-mail-function #'async-smtpmail-send-it))
(after! smtpmail
  (setq smtpmail-smtp-server "smtp.fastmail.com"
        smtpmail-default-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'tls
        smtpmail-queue-mail nil
        smtpmail-queue-dir "~/.mail/queued-mail/"
        smtpmail-servers-requiring-authorization ".*"
        smtpmail-smtp-user user-mail-address))

(when (modulep! :email notmuch)
  (setq +notmuch-sync-backend 'mbsync
        +notmuch-home-function (lambda () (notmuch-search "tag:inbox"))
        +notmuch-mail-folder "~/.mail/fastmail")
  (after! notmuch
    (map! :map notmuch-search-mode-map
          "q" #'cae-notmuch-quit))
  (after! notmuch-hello
    (map! :map notmuch-hello-mode-map
          "q" #'cae-notmuch-quit)))

(when (modulep! :email mu4e)
  (map! [remap compose-mail] #'+mu4e/compose)
  (after! mu4e
    (setq mu4e-contexts
          `(,(make-mu4e-context
              :name "Fastmail"
              :match-func
              (lambda (msg)
                (when msg
                  (string-prefix-p "/Fastmail" (mu4e-message-field msg :maildir))))
              :vars '((user-mail-address . "look@strawberrytea.xyz")
                      (user-full-name . "StrawberryTea")
                      (smtpmail-smtp-server . "smtp.fastmail.com")
                      (smtpmail-default-smtp-server . "smtp.fastmail.com")
                      (smtpmail-stream-type . tls)
                      (smtpmail-smtp-service . 465)
                      (mu4e-trash-folder . "/Fastmail/Trash")
                      (mu4e-refile-folder . "/Fastmail/Archive")
                      (mu4e-drafts-folder . "/Fastmail/Drafts")
                      (mu4e-sent-folder . "/Fastmail/Sent")))))))

;;; HTML

;; I don't really need LSP for the XML files I edit.
(remove-hook 'nxml-mode-local-vars-hook #'lsp!)

;;; Fennel

(when (modulep! :tools lsp)
  (after! lsp-mode
    (setf (alist-get 'fennel-mode lsp-language-id-configuration) "fennel")
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "fennel-ls")
                      :activation-fn (lsp-activate-on "fennel")
                      :server-id 'fennel-ls))))

;;; Appendix

(doom-load-packages-incrementally
 `(,@(when (modulep! :emacs dirvish)
       '(dired transient dirvish))
   auth-source tramp-compat tramp-integration tramp tramp-sh
   ,@(when (modulep! :term eshell)
       '(esh-util esh-module esh-proc esh-io esh-cmd eshell
         em-tramp em-smart em-banner em-basic em-cmpl
         em-extpipe em-glob em-hist em-ls em-script em-term
         em-alias em-elecslash em-rebind em-prompt))
   ,@(when (modulep! :tools pdf)
       '(image-mode pdf-util pdf-info pdf-cache pdf-view pdf-tools)))
 t)

;; Compile Vterm in the background.
(when (modulep! :term vterm)
  (run-with-idle-timer
   3 nil
   (lambda ()
     (unless (require 'vterm-module nil t)
       (async-start
        `(lambda ()
           (add-to-list 'load-path ,(file-name-directory (locate-library "vterm")))
           (require 'vterm)
           (vterm-module-compile))
        (lambda (_)
          (message "vterm module compiled")))))))

(setq cae-config-finished-loading t)

;;Local Variables:
;;eval: (when (featurep 'aggressive-indent) (aggressive-indent-mode -1))
;;End:
