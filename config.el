;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defvar cae-config-finished-loading nil
  "Whether the configuration has finished loading.")

(when cae-config-incremental-compilation-enabled-p
  (run-with-idle-timer 2 nil #'cae-compile-schedule-native-compilation))

;;; Stuff that should not be disabled.

(load! "lisp/cae-bindings")
(load! "lisp/cae-multi")                ;Run parallel Emacs instances.
(load! "lisp/cae-keyboard")             ;Input hacks.
(load! "lisp/cae-smartparens")          ;Allow Smartparens to be disabled. This
                                        ;is also our Smartparens configuration.

(when (and (modulep! :completion helm)
           (modulep! :completion vertico))
  ;; Helm is not our main completion system.
  (remove-hook 'doom-first-input-hook #'helm-mode))

;; Extra stuff for when we run Doom without any modules loaded.
(unless (or (modulep! :completion helm)
            (modulep! :completion ivy)
            (modulep! :completion vertico))
  (load! "lisp/cae-icomplete"))
(unless (modulep! :lang emacs-lisp)
  (remove-hook 'emacs-lisp-mode-hook #'overseer-enable-mode))

;; Stuff so that Emacs doesn't break in the Terminal.
(when (modulep! :completion vertico +childframe)
  (unless (cae-display-graphic-p)
    (remove-hook 'vertico-mode-hook #'vertico-posframe-mode)))
(when (modulep! :ui ligatures)
  (unless (cae-display-graphic-p)
    (remove-hook 'doom-init-ui-hook #'+ligatures-init-buffer-h)))

;;; UI

(when cae-init-ui-enabled-p
  (load! "lisp/cae-theme")
  (after! hydra
    (load! "lisp/cae-cheatsheets"))

  ;; Set up fonts
  (unless (memq system-type '(cygwin windows-nt ms-dos))
    (setq doom-font (font-spec :family "Iosevka Comfy" :size 18)
          doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo"
                                              :size 18)
          doom-unicode-font (unless (modulep! :ui unicode)
                              (font-spec :family "LXGW WenKai" :weight 'light
                                         :size 17))))

  ;; Show absolute line numbers. I prefer to not show relative line numbers
  ;; because I use `avy' commands to jump to lines.
  (setq display-line-numbers-type t)

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

  ;; Colorize color strings.
  (add-hook 'prog-mode-hook #'rainbow-mode)

  (after! ansi-color
    ;; I am trying this out. Not sure if I like it yet.
    (setq ansi-color-bold-is-bright t))

  (setq x-stretch-cursor t              ;Show me if I am on a TAB or a space
        truncate-string-ellipsis "..."  ;The unicode ellipsis is ugly to me
        kill-buffer-delete-auto-save-files t
        scroll-conservatively 0         ;Doom disables this option as a
                                        ;performance optimization but I would
                                        ;much rather have Emacs automatically
                                        ;recenter my windows.
        window-combination-resize t     ;Take new window space from all other
                                        ;windows (not just current)
        scroll-preserve-screen-position 'always)

  (after! time
    (setq display-time-default-load-average nil))

  (after! help-fns
    (setq help-enable-symbol-autoload t))

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
          which-key-max-description-length 35))

  (after! eros
    (setq eros-eval-result-prefix "⟹ ")) ;Pretty arrow

  (after! ffap
    ;; Do not count angle brackets as part of file names because then they get
    ;; mixed up with the tags.
    (setf (alist-get 'nxml-mode ffap-string-at-point-mode-alist)
          (list "--:\\\\${}+@-Z_[:alpha:]~*?#" "" "")))

  ;; Do not spam me with warnings
  (after! warnings
    (setq warning-minimum-level :emergency
          warning-minimum-log-level :emergency))

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
  (add-hook 'debugger-mode-hook #'doom-mark-buffer-as-real-h)

  (use-package! info-colors
    :defer t :init (add-hook 'Info-selection-hook 'info-colors-fontify-node))

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
    (set-popup-rule! "^\\*Backtrace\\*" :size #'+popup-shrink-to-fit :quit nil
      :ttl nil :vslot 99)
    (set-popup-rule! "^\\*exwm" :size #'+popup-shrink-to-fit :ttl nil
      :ttl nil)                         ; which slot/vslot?
    (set-popup-rule! "^\\*Pp Eval Output\\*" :size #'+popup-shrink-to-fit
      :quit nil :ttl t)                 ; which slot/vslot?
    (set-popup-rule! "^\\*org-roam\\*" :size 60 :side 'left :select nil
      :quit nil)                        ; which slot/vslot?
    (set-popup-rule! "^\\*info.*" :size #'cae-popup-resize-help-buffer
      :side 'right :ttl t :select t :quit t :ttl t :slot 2 :vslot 2)
    (set-popup-rule! "^\\*\\(?:Wo\\)?Man " :size #'cae-popup-resize-help-buffer
      :side 'right :ttl t :select t :quit t :ttl 0 :vslot -6)
    (set-popup-rule! "^\\*tldr\\*" :size #'cae-popup-resize-help-buffer
      :side 'right :select t :quit t)   ; which slot/vslot?
    (set-popup-rule! "^\\*\\([Hh]elp\\|Apropos\\)"
      :size #'cae-popup-resize-help-buffer :side 'right :select t :quit t :ttl 0
      :slot 2 :vslot -8)
    (set-popup-rule! "^ \\*Metahelp.*" :size #'cae-popup-resize-help-buffer
      :side 'right :select t :quit t :ttl 0 :slot 2 :vslot -9)
    (set-popup-rule! "^\\*Messages\\*" :vslot -10 :height 10 :side 'bottom
      :select t :quit t :ttl nil :vslot 99)
    (set-popup-rule! "^\\*eww.*" :size #'cae-popup-resize-help-buffer :side 'right
      :select t :ttl nil)               ; which slot/vslot?
    (set-popup-rule! "^\\*w3m\\*$" :size #'cae-popup-resize-help-buffer
      :side 'right :select t :ttl nil)  ; which slot/vslot?
    (set-popup-rule! "^\\*dap-ui-repl\\*$" :vslot -5 :size 0.3 :select t
      :modeline nil :quit nil :ttl nil)
    (set-popup-rule! "^SpeedRect Command Key Help$" :size #'cae-popup-resize-help-buffer
      :side 'right :select nil :quit t :ttl 0) ; which slot/vslot?
    (set-popup-rule! "^\\*ednc-log\\*$" :size #'cae-popup-resize-help-buffer
      :side 'right :select nil :quit t :ttl nil)
    (set-popup-rule! "^\\*tldr\\*$" :size #'cae-popup-resize-help-buffer
      :side 'right :ttl t :select t :quit t :ttl 0) ; which slot/vslot?
    (set-popup-rule! "^\\*Shortdoc .*" :size #'cae-popup-resize-help-buffer
      :side 'right :ttl t :select t :quit t :ttl 0) ; which slot/vslot?
    (set-popup-rule! "^\\*devdocs\\*$" :width 122
      :side 'right :ttl t :select t :quit t :ttl 0) ; which slot/vslot?
    (set-popup-rule! "^Trash Can" :size 0.3 :side 'bottom :select t :quit t
      :ttl 0)                           ; which slot/vslot?
    (after! embark
      (set-popup-rule! (regexp-quote embark--verbose-indicator-buffer)
        :size #'+popup-shrink-to-fit :side 'bottom :ttl t))
    (map! :map messages-buffer-mode-map :n "q" #'quit-window)
    (set-popup-rule! "^\\*notmuch-hello"  :ignore t))

  ;; Lower the default popup delay.
  (after! tooltip
    (setq tooltip-hide-delay 3))

  (when (modulep! :checkers syntax +childframe)
    (after! flycheck-posframe
      (setq flycheck-posframe-border-width 1
            flycheck-posframe-border-use-error-face t)))

  (use-package! goggles
    :defer t :init
    (add-hook 'prog-mode-hook #'goggles-mode)
    (add-hook 'text-mode-hook #'goggles-mode)
    (add-hook 'conf-mode-hook #'goggles-mode)
    :config
    (setq-default goggles-pulse t)
    (when (modulep! :editor multiple-cursors)
      (after! multiple-cursors-core
        (add-to-list 'mc/unsupported-minor-modes #'goggles-mode))))

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
              (defun cae-disable-topsy-in-gptel-h ()
                "Disable topsy-mode in `gptel-mode'." ;`gptel' is Karthink's
                                        ;package.
                (topsy-mode -1))))

  (use-package! anzu
    :defer t :init
    (global-set-key [remap query-replace] 'anzu-query-replace)
    (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
    (define-key isearch-mode-map [remap isearch-query-replace] #'anzu-isearch-query-replace)
    (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
    :config
    (setq anzu-mode-lighter ""
          anzu-replace-threshold 50
          anzu-replace-to-string-separator " → "))

  (use-package! isearch-mb
    :after-call isearch-mode-hook
    :config
    (isearch-mb--setup)
    (isearch-mb-mode +1)
    (add-to-list 'isearch-mb--with-buffer #'recenter-top-bottom)
    (add-to-list 'isearch-mb--with-buffer #'reposition-window)
    (add-to-list 'isearch-mb--with-buffer #'scroll-right)
    (add-to-list 'isearch-mb--with-buffer #'scroll-left)
    (add-to-list 'isearch-mb--with-buffer #'isearch-yank-word)
    (define-key isearch-mb-minibuffer-map (kbd "C-w") #'isearch-yank-word)
    (define-key isearch-mb-minibuffer-map (kbd "C-z j") #'avy-isearch)
    (when (modulep! :completion vertico)
      (add-to-list 'isearch-mb--with-buffer #'consult-isearch-history)
      (map! :map isearch-mb-minibuffer-map
            [remap consult-history] #'consult-isearch-history)
      (add-to-list 'isearch-mb--after-exit #'consult-line)
      (define-key isearch-mb-minibuffer-map (kbd "M-s l") 'consult-line))
    (add-to-list 'isearch-mb--after-exit  #'anzu-isearch-query-replace)
    (add-to-list 'isearch-mb--after-exit  #'anzu-isearch-query-replace-regexp)
    (define-key isearch-mb-minibuffer-map (kbd "M-%")   #'anzu-isearch-query-replace)
    (define-key isearch-mb-minibuffer-map (kbd "M-s %") #'anzu-isearch-query-replace-regexp))

  (use-package! hercules
    :defer t :init
    (after! embark
      (map! :map embark-collect-mode-map
            "<f6>" #'cae-embark-collect-cheatsheet))
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
    (add-hook 'cae-tab-bar-before-switch-hook #'hercules--hide)))

  (use-package! outline-minor-faces
    :defer t :init
    (add-hook 'outline-minor-mode-hook #'outline-minor-faces-mode))


;;; Tools

(when cae-init-tools-enabled-p
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

  (after! browse-url
    (when (getenv "WSL_DISTRO_NAME")
      (setq browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
            browse-url-generic-args     '("/c" "start")))
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

  ;; Set up printers
  (after! lpr (setq printer-name "Brother_HL-L2380DW_series"))
  (after! ps-print (setq ps-printer-name "Brother_HL-L2380DW_series"))

  (setq delete-by-moving-to-trash t
        remote-file-name-inhibit-delete-by-moving-to-trash t
        history-length (expt 2 16)
        make-cursor-line-fully-visible nil ;I forgot why I set this.
        yank-pop-change-selection t)

  (after! xclip
    (cond ((executable-find "termux-setup-storage")
           (setq xclip-method 'termux-clipboard-get))))

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

  (after! projectile
    ;; Automatically find projects in the I personally use.
    (setq projectile-project-search-path
          `((,doom-user-dir . 0)
            (,doom-emacs-dir . 0)
            ,@(when (file-exists-p "~/projects/") '(("~/projects/" . 1)))
            ("~/src/" . 1)))
    (add-to-list 'projectile-globally-ignored-directories
                 (expand-file-name ".local/straight/repos/" user-emacs-directory))
    (unless (or (cl-set-difference projectile-known-projects
                                   '("~/.doom.d/" "~/.emacs.d/" "~/.config/doom/"
                                     "~/.config/emacs/")
                                   :test #'string=)
                (not (file-directory-p "~/src/"))
                (directory-empty-p "~/src/"))
      (projectile-discover-projects-in-search-path))
    ;; Recognize `makefile' as a Makefile.
    (add-to-list
     'projectile-project-types
     '(make marker-files
       ("makefile")
       project-file "Makefile" compilation-dir nil configure-command nil
       compile-command "make" test-command "make test"
       install-command "make install" package-command nil run-command nil)
     nil #'equal)
    (add-to-list
     'projectile-project-types
     '(gnumake marker-files
       ("GNUmakefile")
       project-file "GNUMakefile" compilation-dir nil configure-command nil
       compile-command "make" test-command "make test" install-command
       "make install" package-command nil run-command nil)
     nil #'equal)
    (add-to-list 'projectile-globally-ignored-directories "^.ccls-cache$")
    (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
    (add-to-list 'projectile-project-root-files-top-down-recurring
                 "compile_commands.json")
    ;; Set up compilation.
    (setq projectile-per-project-compilation-buffer t
          compilation-read-command nil)
    ;; Make the project prefix more readable.
    (after! which-key
      (push '((nil . "projectile-\\(.*\\)") . (nil . "\\1"))
            which-key-replacement-alist)))

  (after! compile
    ;; Some projects I work on have many warnings I am not interested in and the
    ;; `first-error' value for `compilation-scroll-output' stops scrolling at the
    ;; first warning. It would be nice if it scrolled to the first error instead.
    ;; Since it doesn't though, I just set it to `t' and scroll up manually if
    ;; there are errors.
    (setq compilation-scroll-output t))

  (after! tramp
    (setq tramp-shell-prompt-pattern "\\(?:^\\|\n\\|\x0d\\)[^]#$%>\n]*#?[]#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*") ;; default + 
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
      (add-to-list 'lsp-disabled-clients 'ccls)
      (add-to-list 'lsp-disabled-clients 'mspyls)))

  (when (modulep! :tools lsp +eglot)
    (after! eglot
      (setf (cdr (assoc '(c++-mode c-mode) eglot-server-programs))
            '("clangd" "--background-index" "--clang-tidy"
              "--completion-style=detailed" "--header-insertion=never"
              "--header-insertion-decorators=0"))))

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
           [remap dired-smart-shell-command] #'dwim-shell-command))))


;;; Editor

(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l"
      doom-leader-key "C-c"
      doom-localleader-key "C-c l")

(when cae-init-editor-enabled-p
  (load! "lisp/cae-repeat")
  (add-hook 'doom-first-buffer-hook
            (cae-defun cae-config-load-visible-mark-h ()
              (load! "lisp/cae-visible-mark")))
  (load! "lisp/cae-vlf")
  (load! "lisp/cae-multiple-cursors")
  (load! "lisp/cae-restore-point")
  (when (modulep! :editor evil)
    (load! "lisp/cae-evil"))

  (autoload 'cae-project-bookmark (concat doom-private-dir
                                          "lisp/cae-project"))
  (autoload 'cae-project-bookmark-set (concat doom-private-dir
                                              "lisp/cae-project"))
  (autoload 'cae-project--get-bookmark-file (concat doom-private-dir
                                                    "lisp/cae-project"))
  (map! :desc "project-bookmark" "C-x r p" #'cae-project-bookmark
        :desc "project-bookmark-set" "C-x r P" #'cae-project-bookmark-set)

  ;; Ensure local elisp packages are up-to-date.
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'cae-compile-rebuild-package nil t)))

  ;; Enable all disabled commands.
  (setq disabled-command-function nil)

  ;; Treat all themes as safe.
  (setq custom-safe-themes t)

  (setq delete-active-region t)         ;makes `d' delete region in Meow.

  (advice-add #'doom/kill-this-buffer-in-all-windows :around #'doom-set-jump-a)
  (advice-add #'kill-buffer-and-window :around #'doom-set-jump-a)

  ;; Query buffers for a diff before killing them.
  ;; (advice-add #'kill-buffer :around #'cae-kill-buffer-a)

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
    (setq expand-region-smart-cursor t)
    (setq er/try-expand-list
          (mapcar (lambda (fn)
                    (if (eq fn #'er/mark-comment)
                        #'cae-mark-comment
                      fn))
                  er/try-expand-list)))

  (setq set-mark-command-repeat-pop t
        next-line-add-newlines t)

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

  (after! ispell
    (setq ispell-quietly t
          ispell-dictionary "en_US"
          ispell-help-in-bufferp 'electric))

  (when (modulep! :emacs undo)
    (after! undo-fu
      (setq undo-fu-allow-undo-in-region t)))

  ;; Hide commands in M-x which do not work in the current mode. Vertico commands
  ;; are hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  (use-package! avy
    :defer t :init
    ;; For some reason this is necessary. It's either a bug in Avy or a bug in the
    ;; fork I'm currently using because I should be able to get this working using
    ;; `avy-styles-alist' instead.
    (advice-add #'avy-goto-end-of-line :around #'cae-avy-use-post-style-a)
    (advice-add #'avy-kill-region :around #'cae-avy-use-pre-style-a)
    (advice-add #'avy-kill-ring-save-region :around #'cae-avy-use-pre-style-a)
    (advice-add #'avy-copy-region :around #'cae-avy-use-pre-style-a)
    (advice-add #'avy-move-region :around #'cae-avy-use-pre-style-a)
    :config
    (setq avy-timeout-seconds 0.4
          avy-all-windows t
          avy-keys (cae-keyboard-remap
                    '(?a ?s ?d ?f ?g
                      ?h ?j ?k ?l ?\;))
          avy-dispatch-alist
          (cae-keyboard-remap
           '((?x . avy-action-kill-move)
             (?X . avy-action-kill-stay)
             (?t . avy-action-teleport)
             (?m . avy-action-mark)
             (?n . avy-action-copy)
             (?y . avy-action-yank)
             (?Y . avy-action-yank-line)
             (?i . avy-action-ispell)
             (?z . avy-action-zap-to-char)))
          avy-styles-alist '((avy-isearch . pre)
                             (ace-link-man . pre)
                             (avy-goto-end-of-line . post)
                             (avy-kill-ring-save-region . pre)
                             (avy-kill-region . pre)
                             (avy-copy-region . pre)
                             (avy-move-region . pre))
          avy-column-line-overlay t))

  (use-package! tabgo
    :commands tabgo
    :defer t :init
    (map! "C-z t" #'tabgo)
    :config
    (setq tabgo-tab-line-keys (cae-keyboard-kbd tabgo-tab-line-keys)))

  (use-package! zop-to-char
    :defer t
    :init (map! [remap zap-to-char] #'zop-up-to-char
                [remap zap-up-to-char] #'zop-to-char)
    :config
    (setq zop-to-char-kill-keys '(?\C-m ?\C-k ?\C-w)))

  ;; I mostly use this package for some additional prefix argument stuff like
  ;; using `C-u - M-:' to insert a string from Elisp without double quotes.
  (use-package! pp+
    :defer t :init
    (defvaralias 'pp-read-expression-map 'minibuffer-local-map)
    (map! [remap eval-last-sexp] #'cae-eval-last-sexp)
    (when (modulep! :tools eval +overlay)
      (after! eros
        (add-hook 'eros-mode-hook
                  (cae-defun cae-eros-setup-keybindings-h ()
                    (map! [remap eval-last-sexp] #'cae-eval-last-sexp))))))

  (use-package! abbrev
    :defer t :config
    (setq-default abbrev-mode t
                  save-abbrevs 'silently)
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
    (add-to-list 'ibuffer-never-show-predicates "^\\*git-auto-push\\*$")
    (add-to-list 'ibuffer-never-show-predicates "^\\*copilot events*\\*$"))

  (use-package! diff-mode
    :defer t :config
    (map! :map diff-mode-map
          "q" #'kill-this-buffer))

  (use-package! aggressive-indent
    :disabled t
    :defer t :init
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode) ;See my `lisp'
                                        ;module.
    (add-hook 'c-mode-common-hook #'aggressive-indent-mode)
    :config
    (add-to-list
     'aggressive-indent-dont-indent-if
     '(and (bound-and-true-p lsp-mode)
           (or (and lsp-enable-on-type-formatting
                    (lsp--capability "documentOnTypeFormattingProvider"))
               (and lsp-enable-indentation
                    (lsp--capability "documentRangeFormattingProvider")))))
    (dolist (command '(lsp-format-buffer
                       lsp-format-region
                       lsp-organize-imports
                       lsp-organize-imports-remove-unused
                       prog-fill-reindent-defun
                       indent-pp-sexp
                       save-buffer
                       indent-for-tab-command))
      (add-to-list 'aggressive-indent-protected-commands command))
    (add-to-list 'aggressive-indent-dont-indent-if '(bound-and-true-p lispy-mode)))

  (use-package! yank-indent
    :defer t :init (add-hook 'doom-first-buffer-hook #'global-yank-indent-mode))

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
    (when (modulep! :editor multiple-cursors)
      (after! multiple-cursors-core
        (add-to-list 'mc/unsupported-minor-modes 'hungry-delete-mode)))
    (add-to-list 'hungry-delete-except-modes 'eshell-mode))

  (use-package! file-info
    :defer t :init
    (map! :leader :prefix "f"
          :desc "Show file info" "i" #'file-info-show)
    :config
    ;; Fixes an error caused by a typo in the code.
    (defvar wakatime-ui-mode nil)
    (defvar wakatime-ui-current-session-text nil)
    ;; See the `:private vc' module for further configuration.
    (setq file-info-include-headlines t
          file-info-max-value-length 100))

  (use-package! titlecase
    :defer t :init
    (after! embark
      (define-key embark-region-map "T" #'titlecase-region)
      (define-key embark-heading-map "T" #'titlecase-line)))

  ;; Type `?' during `rectangle-mark-mode' for a help buffer describing the
  ;; `speedrect' commands.
  (use-package! speedrect
    :after-call rectangle-mark-mode-hook
    :config
    (speedrect-hook))

  (use-package! symbol-overlay
    :defer t :init
    (map! "M-i" #'symbol-overlay-put
          "M-I" #'symbol-overlay-remove-all
          "M-N" #'symbol-overlay-switch-forward ;jump to the next overlay
          "M-P" #'symbol-overlay-switch-backward
          :leader
          :desc "Highlight symbol at point" "to" #'symbol-overlay-mode)
    (add-hook 'prog-mode-hook #'symbol-overlay-mode)
    :config
    (map! :map symbol-overlay-map
          "<f6>" #'cae-symbol-overlay-cheatsheet
          "N" #'symbol-overlay-switch-forward
          "P" #'symbol-overlay-switch-backward
          "r" #'symbol-overlay-rename
          "-" #'negative-argument)
    ;; LSP provides its own symbol highlighting.
    (add-hook 'lsp-mode-hook
              (cae-defun cae-disable-symbol-overlay-h ()
                (symbol-overlay-mode -1)))
    (when (modulep! :editor multiple-cursors)
      ;; Don't distract me while I'm doing multiple cursor calculus.
      (after! multiple-cursors-core
        (add-to-list 'mc/unsupported-minor-modes 'symbol-overlay-mode)))
    (define-key symbol-overlay-map (kbd "o") 'cae-avy-symbol-at-point)
    ;; For some reason `symbol-overlay-switch-backward' jumps to the first symbol
    ;; overlay in the buffer. This is probably a bug.
    (advice-add #'symbol-overlay-get-list
                :around #'cae-hacks-symbol-overlay-reverse-list-a)
    (defun cae-hacks-symbol-overlay-reverse-list-a (oldfun &rest args)
      (if (eq (car args) -1)
          (nreverse (apply oldfun args))
        (apply oldfun args))))

  ;; Make Emacs's sentence commands work with Mr., Mrs., e.g., etc. without
  ;; `sentence-end-double-space'. This package's settings should be tweaked if you
  ;; use multiple languages.
  (use-package! sentex
    :defer t :init
    (if (version<= "30" emacs-version)
        (setq forward-sentence-function #'cae-forward-sentence-function)
      (map! [remap kill-sentence] #'sentex-kill-sentence
            [remap forward-sentence] #'sentex-forward-sentence
            [remap backward-sentence] #'sentex-backward-sentence)))

  (use-package! edit-indirect
    :defer t :init
    (global-set-key (kbd "C-c '") #'cae-edit-indirect-dwim))

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
    ;; Bound in `lisp/cae-bindings'. [remap expand-region] #'eri/expand-region
    :config
    (eri/define-pair org-table-cell "|" 'org-at-table-p)
    (eri/add-mode-expansions 'org-mode
      '((eri/mark-inside-org-table-cell
         eri/mark-outside-org-table-cell))))

  (use-package! switchy-window
    :defer t :init
    (add-hook 'doom-first-buffer-hook #'switchy-window-minor-mode)
    :config
    (keymap-set switchy-window-minor-mode-map
                "<remap> <other-window>" #'switchy-window)))


;;; Autocompletion

(when cae-init-autocompletion-enabled-p
  (when (modulep! :private corfu)
    (load! "lisp/cae-corfu"))

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
    :defer t :init
    (map! [remap dabbrev-expand] #'hippie-expand)
    :config
    (setq  hippie-expand-try-functions-list
           '(try-expand-dabbrev
             try-expand-dabbrev-all-buffers
             try-expand-dabbrev-from-kill
             try-complete-file-name-partially
             try-complete-file-name
             try-complete-lisp-symbol-partially
             try-complete-lisp-symbol
             try-expand-line)))

  (use-package! copilot
    :defer t :init
    (add-hook 'text-mode-hook   #'copilot-mode)
    (add-hook 'prog-mode-hook   #'copilot-mode)
    (add-hook 'conf-mode-hook   #'copilot-mode)
    (add-hook 'minibuffer-setup-hook #'copilot-mode)
    :config
    (setq copilot--base-dir
          (expand-file-name ".local/straight/repos/copilot.el/" doom-emacs-dir))
    ;; Model our Copilot interface after Fish completions.
    (map! :map copilot-completion-map
          "<right>" #'copilot-accept-completion
          "C-f" #'copilot-accept-completion
          "M-<right>" #'copilot-accept-completion-by-word
          "M-f" #'copilot-accept-completion-by-word
          "C-e" #'copilot-accept-completion-by-line
          "<end>" #'copilot-accept-completion-by-line
          "M-n" #'copilot-next-completion
          "M-p" #'copilot-previous-completion)
    (defun cae-copilot-clear-overlay-h ()
      "Like `copilot-clear-overlay', but returns `t' if the overlay was visible."
      (when (copilot--overlay-visible)
        (copilot-clear-overlay) t))
    (add-hook 'doom-escape-hook #'cae-copilot-clear-overlay-h)
    (add-to-list 'copilot-disable-predicates
                 (cae-defun cae-disable-copilot-in-gptel-p ()
                   (bound-and-true-p gptel-mode)))
    (when (modulep! :editor snippets)
      (add-hook 'yas-before-expand-snippet-hook #'copilot-clear-overlay))
    (when (modulep! :editor multiple-cursors)
      (add-to-list 'copilot-disable-predicates
                   (cae-defun cae-multiple-cursors-active-p ()
                     (bound-and-true-p multiple-cursors-mode)))))

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
  ;; Allow us to type HEAD~1, HEAD~2, etc. as arguments to git commands.
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
    (add-to-list '+ligatures-in-modes 'org-mode t))

  (after! which-key
    (which-key-add-keymap-based-replacements org-mode-map
      "C-c \"" "plot"
      "C-c C-v" "org-babel-map")))

;;; AI

(use-package! org-ai
  :defer t :init
  (map! "C-c M-a" #'cae-ai-lazy-load-org-ai)
  (autoload 'org-ai-mode "org-ai" nil t)
  (add-hook 'org-mode-hook #'org-ai-mode)
  :config
  (map! :map org-ai-global-mode-map
        :prefix "C-c M-a"
        "b" #'cae-ai-org-ai-on-buffer)
  (map! :map org-ai-mode-map
        [remap org-ai-kill-region-at-point] #'cae-ai-org-ai-kill-region-at-point)
  (defvar org-ai-global-mode-prefix-map
    (lookup-key org-ai-global-mode-map (kbd "C-c M-a")))
  (org-ai-global-mode +1)
  (setq org-ai-default-chat-model "gpt-4")
  (when (modulep! :editor snippets)
    (org-ai-install-yasnippets)))

;;; Email

(setq user-full-name "StrawberryTea"
      user-mail-address "look@strawberrytea.xyz"
      mail-host-address "strawberrytea.xyz")

(when (modulep! :email notmuch)
  (setq +notmuch-sync-backend 'mbsync
        +notmuch-home-function (lambda () (notmuch-search "tag:inbox")))
  (after! notmuch
    (map! :map notmuch-search-mode-map
          "q" #'cae-notmuch-quit))
  (after! notmuch-hello
    (map! :map notmuch-hello-mode-map
          "q" #'cae-notmuch-quit)))

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

(setq cae-config-finished-loading t)

;;Local Variables:
;;eval: (when (featurep 'aggressive-indent) (aggressive-indent-mode -1))
;;End:
