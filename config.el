;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defvar cae-config-finished-loading nil)

;;; Stuff that should not be disabled.

;; I have disabled `rainbow-delimiters' as a package so this is to prevent
;; errors.
(defalias 'rainbow-delimiters-mode #'ignore)

(load! "lisp/cae-tty")
(load! "lisp/cae-bindings")
(load! "lisp/cae-multi")                ;Run parallel Emacs instances.
(load! "lisp/cae-smartparens")          ;Allow Smartparens to be disabled. This
                                        ;is also our Smartparens configuration.
(load! "lisp/cae-projectile")           ;Allow Projectile to be disabled. This
                                        ;is also our Projectile configuration.
(when (modulep! :editor evil)
  (load! "lisp/cae-evil"))

;; Helm is not our main completion system.
(when (and (modulep! :completion helm)
           (modulep! :completion vertico))
  (remove-hook 'doom-first-input-hook #'helm-mode))

(unless (or (modulep! :completion helm)
            (modulep! :completion ivy)
            (modulep! :completion vertico))
  (icomplete-mode +1)
  (icomplete-vertical-mode +1))
(unless (modulep! :lang emacs-lisp)
  (remove-hook 'emacs-lisp-mode-hook #'overseer-enable-mode))

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
  (setq doom-font (font-spec :family "Iosevka Comfy" :size 18)
        doom-serif-font (font-spec :family "IBM Plex Mono" :size 18 :weight 'light)
        doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo"
                                            :size 18)))

;; Do not break my clipboard in SSH sessions.
(when (and (modulep! :os tty)
           (getenv "SSH_TTY")
           (not (cae-display-graphic-p)))
  (remove-hook 'tty-setup-hook #'doom-init-clipboard-in-tty-emacs-h))

(after! tramp
  (setq tramp-shell-prompt-pattern
        "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))

(after! nsm
  (setq network-security-level 'high))


;;; UI

(when cae-init-ui-enabled-p
  (when (cae-display-graphic-p)
    (load! "lisp/cae-theme"))
  (when (modulep! :ui doom-dashboard)
    (load! "lisp/cae-dashboard"))
  (after! hydra
    (load! "lisp/cae-hydra"))

  ;; Show absolute line numbers. I prefer to not show relative line numbers
  ;; because I use `avy' commands to jump to lines.
  (setq display-line-numbers-type t
        display-line-numbers-width-start t)

  ;; Based on the documentation, estimate for what this magic number should be
  ;; (no benchmarks) is 10000. It's also the value used in `emacs-ess'.
  (setq jit-lock-chunk-size 10000)

  ;; Show minibuffer recursion depth
  (autoload 'minibuffer-depth-setup "mb-depth")
  (add-hook 'minibuffer-setup-hook  #'minibuffer-depth-setup)

  (add-hook 'tabulated-list-mode-hook #'hl-line-mode)

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
  (setq tab-bar-show 1)

  ;; Don't confirm when exiting Emacs that active processes exist.
  (setq confirm-kill-processes nil)

  (setq x-stretch-cursor t              ;Show me if I am on a TAB or a space
        kill-buffer-delete-auto-save-files t
        window-combination-resize t     ;Take new window space from all other
                                        ;windows (not just current)
        scroll-preserve-screen-position 'always
        suggest-key-bindings nil
        cursor-in-non-selected-windows t)

  (setq set-message-functions
        '(inhibit-message
          set-minibuffer-message)
        inhibit-message-regexps '("C-g is undefined$"
                                  "ESC is undefined$"))

  (after! flymake
    (setq flymake-start-on-flymake-mode nil))

  ;; A little bit of margin is nice but I don't like it when I'm in terminal
  ;; popups.
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

  (after! doom-modeline
    (setq doom-modeline-hud t
          doom-modeline-support-imenu t
          doom-modeline-mu4e t
          doom-modeline-gnus t
          doom-modeline-github t
          doom-modeline-major-mode-icon nil
          doom-modeline-minor-modes nil)
    (after! mu4e
      (setq mu4e-modeline-show-global t)))

  (after! which-key
    (setq which-key-ellipsis "..."
          which-key-idle-delay 0.5
          which-key-compute-remaps t
          which-key-max-description-length 35
          which-key-separator (if (cae-tty-disable-unicode-p) " -> " " → "))
    (pushnew!
     which-key-replacement-alist
     '(("" . "\\`+?evil[-:/]?\\(?:a-\\)?\\(.*\\)") . (nil . "\\1"))
     '(("" . "winum-\\(.*\\)") . (nil . "\\1"))
     '(("" . "+workspace[-/]\\(.*\\)") . (nil . "\\1"))
     '(("" . "doom[-/]\\(.*\\)") . (nil . "\\1"))
     '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "\\1"))))

  (setq +zen-text-scale 0.8)

  (after! eros
    (setq eros-eval-result-prefix (if (cae-tty-disable-unicode-p) "=> " "⟹ ")))

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
                (setq warning-minimum-level :error
                      warning-minimum-log-level :error))
              :append))

  (after! alert
    (setq alert-default-style 'libnotify))

  (after! shr
    ;; `shr' wraps lines in a visually unappealing way.
    (setq shr-width 120
          shr-max-width 120)

    ;; I prefer to not use fonts in `shr' because it looks weird with the font setup I have.
    (setq shr-use-fonts nil)

    ;; Sometimes EWW makes web pages unreadable by adding a bright background. Do
    ;; not colorize backgrounds at all.
    (advice-add #'shr-colorize-region :around #'ignore))

  ;; Allow switching to these buffers with `C-x b'
  (add-hook 'compilation-mode-hook #'doom-mark-buffer-as-real-h)

  ;; ANSI colors
  (add-hook 'text-mode-hook
            (cae-defun cae-ansi-color-text-mode-h ()
              (unless (derived-mode-p 'org-mode)
                ;; Apply ANSI color codes
                (with-silent-modifications
                  (ansi-color-apply-on-region (point-min) (point-max) t)))))
  (setenv "TERM" "xterm-256color")
  (setq compilation-environment '("TERM=xterm-256color"))

  (use-package! info-colors
    :defer t :init (add-hook 'Info-selection-hook #'info-colors-fontify-node))

  (use-package! authinfo-color-mode
    :defer t :init
    (add-to-list 'auto-mode-alist '("authinfo.gpg\\'" . authinfo-color-mode))
    (add-to-list 'auto-mode-alist '("authinfo\\'" . authinfo-color-mode))
    (advice-add 'authinfo-mode :override #'authinfo-color-mode))

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
        ("^\\*ielm\\*$" :size cae-popup-resize-help-buffer
         :side right :ttl nil :select t :quit t :ttl 0)
        ("^\\*devdocs\\*$" :width 122
         :side right :ttl t :select t :quit t :ttl 0)
        ("^Trash Can" :size 0.3 :side bottom :select t :quit t
         :ttl 0)
        ("^\\*evil-owl\\*$" :side bottom :select nil :ttl 0 :size cae-popup-shrink-to-fit)
        ("^\\*chatgpt\\* " :size 0.3 :select t :quit nil :ttl nil)
        ("^\\*vterm" :quit nil :ttl nil :size 0.3)
        ("^\\*notmuch-hello"  :ignore)
        ("^\\*gud-" :ttl nil :size 0.35)
        ("embrace-help" :side top)
        ("*Neato Graph Bar" :side top :quit t :ttl 0 :size
         (lambda (win) (set-window-text-height win (+ (num-processors) 2))))))
    (after! embark
      (set-popup-rule! (regexp-quote embark--verbose-indicator-buffer)
        :size #'+popup-shrink-to-fit :side 'bottom :ttl t)
      (set-popup-rule! "^\\*Embark Export: "
        :size #'cae-popup-resize-help-buffer :side 'right :ttl 0))
    (after! elfeed
      (set-popup-rule! (format "^%s$" (regexp-quote elfeed-log-buffer-name))
        :size '+popup-shrink-to-fit :side 'right :select nil :quit t :ttl nil))
    (after! bbdb
      (set-popup-rule! (format "^%s$" (regexp-quote bbdb-buffer-name))
        :select nil :quit t :ttl nil))
    (map! :map messages-buffer-mode-map :n "q" #'quit-window))

  ;; Lower the default popup delay.
  (after! tooltip
    (setq tooltip-hide-delay 3))

  (after! flycheck-posframe
    (setq flycheck-posframe-border-width 1
          flycheck-posframe-border-use-error-face t))

  ;; Show the window number in the modeline (when applicable).
  (setq winum-auto-setup-mode-line t)

  (use-package! breadcrumb
    :unless (modulep! :ui modeline)
    :defer t :init
    (add-hook 'doom-first-buffer-hook #'breadcrumb-mode))

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
          beacon-overlay-priority -1)
    (dolist (cmd '(+eshell-tldr-to-man))
      (add-to-list 'beacon-dont-blink-commands cmd)))

  (use-package! outline-minor-faces
    :defer t :init
    (add-hook 'outline-minor-mode-hook #'outline-minor-faces-mode))

  (use-package indent-bars
    :custom
    (indent-bars-treesit-support t)
    (indent-bars-no-descend-string t)
    (indent-bars-treesit-ignore-blank-lines-types '("module"))
    (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
                                 list list_comprehension
                                 dictionary dictionary_comprehension
                                 parenthesized_expression subscript)))
    :hook ((python-base-mode yaml-mode) . indent-bars-mode))

  (use-package! nice-citation
    :when (cae-display-graphic-p)
    :after (:or gnus message))

  (use-package! backline
    :defer t :init
    (advice-add 'outline-flag-region :after 'backline-update)))


;;; Tools

(when cae-init-tools-enabled-p
  ;; Set up the default browser.
  (after! browse-url
    (setq browse-url-browser-function
          (cond ((executable-find "termux-setup-storage")
                 #'browse-url-xdg-open)
                (t #'browse-url-generic))
          browse-url-secondary-browser-function
          #'eww-browse-url)
    (cond ((getenv "WSL_DISTRO_NAME")
           (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
                 browse-url-generic-args '("/c" "start")))
          ((when-let ((chrome (or (executable-find "chromium-bin-browser")
                                  (executable-find "google-chrome-unstable")
                                  (executable-find "google-chrome-stable"))))
             (setq browse-url-generic-program chrome
                   browse-url-generic-args (when (eq (user-uid) 0)
                                             '("--no-sandbox")))))
          ((executable-find "firefox-bin")
           (setq browse-url-generic-program "firefox-bin"))))

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
  (add-hook 'syslog-mode-hook #'cae-apply-ansi-color-to-buffer-h)

  ;; Set up printers.
  (after! lpr (setq printer-name "Brother_HL-2350DW"))
  (after! ps-print (setq ps-printer-name "Brother_HL-2350DW"))
  (after! pdf-misc (setq pdf-misc-print-program-executable (executable-find "lpr")))

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

    ;; Since it doesn't though, I just set it to `t' and scroll up manually if
    ;; there are errors.
    (setq compilation-scroll-output t))

  (after! tramp
    (setq tramp-use-scp-direct-remote-copying t)
    (dolist (path '("~/.guix-profile/bin" "~/.guix-profile/sbin"
                    "/run/current-system/profile/bin"
                    "/run/current-system/profile/sbin"))
      (add-to-list 'tramp-remote-path path)))

  (after! marginalia
    ;; Use `embark-file-map' for `ffap-menu'.
    (add-to-list 'marginalia-prompt-categories '("\\<find file\\>" . file)))
  (add-hook 'vertico-mode-hook #'vertico-multiform-mode)
  (add-hook 'vertico-mode-hook #'vertico-mouse-mode)
  (remove-hook 'vertico-mode-hook #'vertico-posframe-mode)
  (after! vertico-multiform
    (setq vertico-multiform-categories
          `((embark-keybinding grid)
            (consult-grep
             ,(if (and (cae-display-graphic-p)
                       (modulep! :completion vertico +childframe))
                  'posframe 'buffer))
            (imenu ,@(if (and (cae-display-graphic-p)
                              (modulep! :completion vertico +childframe))
                         '(posframe grid) '(grid)))
            (consult-location ,(if (and (cae-display-graphic-p)
                                        (modulep! :completion vertico +childframe))
                                   'posframe 'buffer))
            ,@(if (cae-display-graphic-p)
                  (if (modulep! :completion vertico +childframe)
                      (list t 'posframe)
                    nil)
                (list t 'flat)))))

  ;; Use Emacs as the default editor for shell commands.
  (when (cae-display-graphic-p)
    (dolist (hook '(shell-mode-hook eshell-mode-hook vterm-mode-hook))
      (dolist (fn '(with-editor-export-editor
                    with-editor-export-hg-editor
                    with-editor-export-git-editor))
        (add-hook hook fn)))
    (advice-add #'with-editor-export-editor :around #'cae-hacks-shut-up-a))

  (after! spell-fu
    (add-to-list 'spell-fu-faces-exclude 'message-header-other)
    (add-to-list 'spell-fu-faces-exclude 'org-property-value)
    (add-to-list 'spell-fu-faces-exclude 'message-header-to)
    (setq spell-fu-faces-exclude
          (delq 'font-lock-string-face spell-fu-faces-include)))

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
    (add-to-list 'lsp-disabled-clients 'mspyls))

  (after! eglot
    (let ((clangd '("clangd" "--background-index" "--clang-tidy"
                    "--completion-style=detailed" "--header-insertion=never"
                    "--header-insertion-decorators=0")))
      (if (assoc '(c++-mode c-mode) eglot-server-programs)
          (setf (cdr (assoc '(c++-mode c-mode) eglot-server-programs)) clangd)
        (setq eglot-server-programs
              (cons (cons '(c++-mode c-mode) clangd)
                    eglot-server-programs)))))

  (after! spell-fu
    (add-hook 'nxml-mode-hook
              (cae-defun cae-disable-spell-fu-h ()
                (spell-fu-mode -1))))

  (use-package! 0x0
    :defer t :init
    (map! "C-x U" #'0x0-dwim)
    (after! embark
      (define-key embark-region-map (kbd "U") '0x0-dwim)))

  (use-package! wakatime-mode
    :defer t :defer-incrementally t :config
    (global-wakatime-mode +1)
    (setq wakatime-cli-path (executable-find "wakatime")))

  
  ;; Loading `tramp-sh' is slow, so we have this hook load auto-sudoedit if we need
  ;; to use sudo on a file before `tramp-sh' is loaded.
  (add-hook 'find-file-hook #'cae-auto-sudoedit-maybe-h -1)
  (use-package! auto-sudoedit
    :after tramp-sh :config
    (remove-hook 'find-file-hook #'cae-auto-sudoedit-maybe-h)
    (advice-add #'dirvish-data-for-dir :around
                (cae-defun cae-auto-sudoedit-file-local-name-a (oldfun dir buffer setup)
                  (funcall oldfun (tramp-file-local-name dir) buffer setup)))
    (auto-sudoedit-mode +1)))


;;; Editor

(when cae-init-editor-enabled-p
  (load! "lisp/cae-repeat")
  (load! "lisp/cae-vlf")
  (load! "lisp/cae-restore-point")
  (when (and (modulep! :editor multiple-cursors)
             (not (modulep! :editor evil)))
    (load! "lisp/cae-multiple-cursors"))
  (when (or (modulep! :private helm)
            (modulep! :completion helm))
    (load! "lisp/cae-helm"))

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

  ;; Skip some buffers with prev buffer
  (setq switch-to-prev-buffer-skip
        (lambda (_win buf _bury-or-kill)
          (or (get-buffer-window buf)
              (doom-unreal-buffer-p buf))))

  (setq delete-active-region t)         ;makes `d' delete region in Meow.

  ;; Allow us to undo deleting frames.
  (undelete-frame-mode +1)

  ;; Automatically deindent items when they are added to the kill ring.
  (add-hook #'doom-first-input-hook #'kill-ring-deindent-mode)

  ;; Navigate camelCase words.
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

  ;; Allow C-u - using `pp' on the `eval-expression' output.
  (defvaralias 'pp-read-expression-map 'minibuffer-local-map)
  (map! [remap eval-last-sexp] #'cae-eval-last-sexp
        [remap eval-expression] #'cae-eval-expression)
  (add-hook 'eros-mode-hook
            (cae-defun cae-eros-setup-keybindings-h ()
              (map! [remap eval-last-sexp] #'cae-eval-last-sexp)))

  (after! outline
    (setq outline-minor-mode-use-buttons nil))

  (after! ispell
    (setq ispell-quietly t
          ispell-dictionary "en_US"
          ispell-help-in-bufferp 'electric))

  (after! vline
    (setq vline-idle-time 0.1))

  (after! undo-fu
    (setq undo-fu-allow-undo-in-region t))

  ;; Hide commands in M-x which do not work in the current mode. Vertico commands
  ;; are hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  (after! embark
    (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target))

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
          avy-background nil
          avy-dispatch-alist
          (cae-keyboard-remap
           '((?z . avy-action-zap-to-char)
             (?x . cae-avy-action-kill)
             (?e . cae-avy-action-embark-act)
             (?r . cae-avy-parrot-rotate)
             (?R . cae-avy-parrot-rotate)
             (?\; . cae-avy-action-comment-dwim)))
          avy-styles-alist '((avy-isearch . pre)
                             (ace-link-man . pre)
                             (avy-goto-end-of-line . post)
                             (avy-kill-ring-save-region . pre)
                             (avy-kill-region . pre)
                             (avy-copy-region . pre)
                             (avy-move-region . pre))
          avy-column-line-overlay t))

  (use-package! embrace
    :defer t :init
    ;; Respect popup rules with the Embrace help popup.
    (advice-add #'embrace--show-help-buffer :around
                (cae-defun +cae-embrace-use-popup-a (oldfun help-string)
                  (cl-letf (((symbol-function #'display-buffer-in-side-window)
                             (symbol-function #'display-buffer)))
                    (funcall oldfun help-string))))
    ;; I don't like Doom's `f' for a Lisp function. I prefer `C-f' and `f' to
    ;; behave the same way globally rather than having `f' be context-sensitive.
    (remove-hook! (lisp-mode emacs-lisp-mode clojure-mode racket-mode hy-mode)
      #'+evil-embrace-lisp-mode-hook-h)
    (defadvice! cae-embrace-init-pairs-a (&rest args)
      :after #'embrace--setup-defaults
      ;; Switch opening and closing pairs to match `evil-surround'.
      (dolist (pair '((?\} . ("{" . "}"))
                      (?\{ . ("{ " . " }"))
                      (?\( . ("( " . " )"))
                      (?\) . ("(" . ")"))
                      (?\[ . ("[ " . " ]"))
                      (?\] . ("[" . "]"))
                      (?< . ("< " . " >"))
                      (?> . ("<" . ">"))))
        (embrace-add-pair (car pair) (cadr pair) (cddr pair)))
      (embrace-add-pair-regexp ?\C-f "(\\([^ ]+\\) " ")" 'cae-embrace-with-prefix-function
                               (embrace-build-help "(function " ")")))
    :config
    (after! evil-embrace
      (setq evil-embrace-show-help-p t))
    (setq embrace-show-help-p t))

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

  (after! outline
    (setq outline-font-lock-faces nil
          outline-font-lock-keywords nil)
    (after! which-key
      (which-key-add-keymap-based-replacements outline-minor-mode-map
        "C-c @" "outline")))

  (use-package! embark
    :defer t :config
    (map! :map embark-collect-mode-map "<f6>" #'cae-cheatsheets-embark-collect)
    (after! vertico
      (define-key vertico-map (kbd "C-z") 'cae-embark-act-with-completing-read))
    (advice-add #'embark-completing-read-prompter :around
                #'cae-bind-C-z-to-abort-a))

  (use-package! logos
    :defer t :init
    (map! [remap forward-page] #'logos-forward-page-dwim
          [remap backward-page] #'logos-backward-page-dwim
          [remap narrow-to-page] #'cae-narrow-to-page)
    :config
    (setq logos-outlines-are-pages t))

  (use-package! aas
    :defer t :init
    (add-hook 'doom-first-input-hook #'aas-global-mode)
    :config
    (advice-add #'aas-embark-menu :before
                (cae-defun cae-aas-load-embark-h ()
                  (require 'embark)))
    (aas-set-snippets 'global
      ";--" "—"
      ";-." "→"
      ";=." "⇒"
      ";!=" "≠"
      "-." "->"
      "=." "=>"))

  (use-package! smart-semicolon
    :defer t :init
    (add-hook 'c-mode-common-hook #'smart-semicolon-mode)
    (add-hook 'web-mode-hook  #'smart-semicolon-mode)
    (add-hook 'java-mode-hook #'smart-semicolon-mode)
    (add-hook 'js-mode-hook   #'smart-semicolon-mode))

  (use-package! expand-region-improved
    :defer t :init
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

  (use-package! parrot
    :defer t :init
    ;; Wrangle parrot into being fully lazy-loaded.
    (autoload #'parrot-party-while-process "parrot")
    (autoload #'parrot--todo-party "parrot")
    (autoload #'parrot--magit-push-filter "parrot")
    (advice-add #'gac-push
                :after
                (cae-defun cae-modeline-gac-party-on-push-a (buffer)
                  (when-let ((proc (get-buffer-process "*git-auto-push*")))
                    (parrot-party-while-process proc))))
    (add-hook 'org-after-todo-state-change-hook #'parrot--todo-party)
    (advice-add 'magit-run-git-async :around #'parrot--magit-push-filter)
    :config
    (setq parrot-animate 'hide-static
          parrot-num-rotations 3
          parrot-animate-on-load nil
          parrot-party-on-magit-push t
          parrot-party-on-org-todo-states '("DONE")
          parrot-type 'nyan)
    (parrot-mode +1))

  (use-package! parrot-rotate
    :defer t :init
    (map! :n "]r"  #'cae-modeline-rotate-forward-word-at-point
          :n "[r"  #'cae-modeline-rotate-backward-word-at-point)
    :config
    (after! parrot-rotate
      (setq parrot-rotate-animate-after-rotation nil
            parrot-rotate-highlight-after-rotation nil
            parrot-rotate-start-bound-regexp "[\]\[[:space:](){}<>]"
            parrot-rotate-end-bound-regexp "[\]\[[:space:](){}<>]")
      (add-to-list 'parrot-rotate-dict '(:rot ("add-hook" "remove-hook")))
      (add-to-list 'parrot-rotate-dict '(:rot ("add-hook!" "remove-hook!")))
      (add-to-list 'parrot-rotate-dict '(:rot ("Yes" "No")))
      (add-to-list 'parrot-rotate-dict '(:rot ("nil" "t")))
      (add-to-list 'parrot-rotate-dict '(:rot ("advice-add" "advice-remove")))
      (add-to-list 'parrot-rotate-dict '(:rot ("defadvice!" "undefadvice!")))
      (add-to-list 'parrot-rotate-dict '(:rot ("cae-keyboard-remap"
                                               "cae-keyboard-remap-to-strings"
                                               "cae-keyboard-strings")))
      (add-to-list 'parrot-rotate-dict '(:rot ("kbd" "cae-keyboard-kbd")))
      (add-to-list 'parrot-rotate-dict '(:rot ("+log" "message")))
      (add-to-list 'parrot-rotate-dict '(:rot ("backtrace!" "unbacktrace!")))
      (add-to-list 'parrot-rotate-dict '(:rot ("enabled" "disabled")))))

  (use-package! string-inflection
    :commands (string-inflection-all-cycle
               string-inflection-toggle
               string-inflection-camelcase
               string-inflection-lower-camelcase
               string-inflection-kebab-case
               string-inflection-underscore
               string-inflection-capital-underscore
               string-inflection-upcase)
    :init
    (map! :leader :prefix-map ("c~" . "naming convention")
          :desc "cycle" "~" #'string-inflection-all-cycle
          :desc "toggle" "t" #'string-inflection-toggle
          :desc "CamelCase" "c" #'string-inflection-camelcase
          :desc "downCase" "d" #'string-inflection-lower-camelcase
          :desc "kebab-case" "k" #'string-inflection-kebab-case
          :desc "under_score" "_" #'string-inflection-underscore
          :desc "Upper_Score" "u" #'string-inflection-capital-underscore
          :desc "UP_CASE" "U" #'string-inflection-upcase)
    (after! evil
      (evil-define-operator evil-operator-string-inflection (beg end _type)
        "Define a new evil operator that cycles symbol casing."
        :move-point nil
        (interactive "<R>")
        (string-inflection-all-cycle)
        (setq evil-repeat-info '([?g ?~])))
      (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)))

  (use-package! beginend
    :defer t :init
    (add-hook 'doom-first-input-hook #'beginend-global-mode)
    ;; This patches around this function not being compatible with Evil when
    ;; `evil-move-beyond-eol' is `nil'.
    (advice-add #'beginend-prog-mode-goto-end :before #'end-of-line))

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
    (define-key isearch-mb-minibuffer-map (kbd "M-s %") #'anzu-isearch-query-replace-regexp)))


;;; Autocompletion

(when cae-init-autocompletion-enabled-p
  (when (modulep! :completion corfu)
    (load! "lisp/cae-corfu"))

  (setq ido-save-directory-list-file (concat doom-cache-dir "ido.last"))
  (after! ido
    (load! "lisp/cae-ido"))

  (after! yasnippet
    (setq yas-triggers-in-field t     ;Allow nested snippets.
          yas-trigger-symbol (if (cae-tty-disable-unicode-p) " =>"" →")))

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
    (add-to-list 'consult-preview-allowed-hooks 'global-org-modern-mode-check-buffers)
    (add-to-list 'consult-preview-allowed-hooks 'global-hl-todo-mode-check-buffers)
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
    (advice-add #'register-preview :override #'consult-register-window)))


;;; Term

(when cae-init-term-enabled-p
  ;; Enable Fish autocompletion in `read-shell-command'.
  (advice-add #'shell-completion-vars :after #'fish-completion-mode)

  (after! em-glob
    ;; Allow us to type HEAD~1, HEAD~2, etc., as arguments to git commands.
    (setq eshell-error-if-no-glob nil))

  (after! vterm
    (setq vterm-max-scrollback 100000))

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

  ;; Do not spam my terminal with escape sequences when `wal' exports colors.
  (when (executable-find "wal")
    (add-hook 'comint-preoutput-filter-functions
              (cae-defun cae-filter-wal-sequences (s)
                (if (string-prefix-p "]4;0;#" s)
                    "" s)))))


;;; Text

(when cae-init-text-enabled-p
  ;;(remove-hook 'text-mode-hook #'visual-line-mode)
  ;;(add-hook 'text-mode-hook #'auto-fill-mode)
  ;;(remove-hook 'text-mode-hook #'display-line-numbers-mode)
  ;;
  ;;;; Margin in text mode without line numbers.
  ;;(defvar +text-mode-left-margin-width 1
  ;;  "The `left-margin-width' to be used in `text-mode' buffers.")
  ;;
  ;;(defun +setup-text-mode-left-margin ()
  ;;  (when (and (derived-mode-p 'text-mode)
  ;;             (not (and (bound-and-true-p visual-fill-column-mode)
  ;;                       visual-fill-column-center-text))
  ;;             (eq (current-buffer)     ; Check current buffer is active.
  ;;                 (window-buffer (frame-selected-window))))
  ;;    (setq left-margin-width (if display-line-numbers
  ;;                                0 +text-mode-left-margin-width))
  ;;    (set-window-buffer (get-buffer-window (current-buffer))
  ;;                       (current-buffer))))
  ;;
  ;;(remove-hook 'window-configuration-change-hook #'+setup-text-mode-left-margin)
  ;;(remove-hook 'display-line-numbers-mode-hook #'+setup-text-mode-left-margin)
  ;;(remove-hook 'text-mode-hook #'+setup-text-mode-left-margin)
  ;;
  ;;(defadvice! +doom/toggle-line-numbers--call-hook-a ()
  ;;  :after #'doom/toggle-line-numbers
  ;;  (run-hooks 'display-line-numbers-mode-hook))

  (after! calendar
    (setq calendar-week-start-day 1
          calendar-mark-diary-entries-flag t))

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
          org-special-ctrl-k t
          org-highlight-latex-and-related nil
          org-priority-highest ?A
          org-priority-lowest ?E
          org-priority-faces
          '((?A . 'nerd-icons-red)
            (?B . 'nerd-icons-orange)
            (?C . 'nerd-icons-yellow)
            (?D . 'nerd-icons-green)
            (?E . 'nerd-icons-blue))
          org-agenda-deadline-faces
          '((1.001 . error)
            (1.0 . org-warning)
            (0.5 . org-upcoming-deadline)
            (0.0 . org-upcoming-distant-deadline))
          ;; All my computers use 64-bit processors
          org-read-date-force-compatible-dates nil)
    (when (modulep! :lang org +roam2)
      (setq +org-roam-auto-backlinks-buffer nil))
    (map! :map org-mode-map
          "C-c C-M-h" #'er/mark-org-code-block)
    (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
    (after! org-list
      (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))))
    (after! ob-core
      ;; Export commments by default.
      (setq org-babel-default-header-args
            '((:session . "none")
              (:results . "replace")
              (:exports . "code")
              (:cache . "no")
              (:noweb . "no")
              (:hlines . "no")
              (:tangle . "no")
              (:comments . "link"))))
    (after! ox
      (setq org-export-allow-bind-keywords t))
    (after! org-crypt
      (setq org-crypt-disable-auto-save 'encrypt))
    (after! org-agenda
      (setq org-agenda-sticky nil
            org-agenda-files '("~/org/")))

    (after! which-key
      (which-key-add-keymap-based-replacements org-mode-map
        "C-c \"" "plot"
        "C-c C-v" "org-babel-map"))))


;;; Email

(when cae-init-email-enabled-p
  (setq user-full-name "StrawberryTea"
        user-mail-address "look@strawberrytea.xyz"
        mail-host-address "strawberrytea.xyz"
        mail-source-directory "~/.mail/")

  (autoload 'async-smtpmail-send-it "smtpmail-async" nil t)
  (setq compose-mail-user-agent-warnings nil)
  (after! sendmail
    (setq send-mail-function #'async-smtpmail-send-it
          mail-self-blind t))
  (after! message
    (setq message-send-mail-function #'async-smtpmail-send-it
          message-default-mail-headers "Bcc: look@strawberrytea.xyz\n"
          message-forward-as-mime t
          message-forward-before-signature t)
    (add-hook 'message-setup-hook #'message-check-recipients))
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
        +notmuch-home-function (lambda () (notmuch-search "tag:inbox"))
        +notmuch-mail-folder "~/.mail/fastmail")
  (after! notmuch
    (map! :map notmuch-search-mode-map
          "q" #'cae-notmuch-quit))
  (after! notmuch-hello
    (map! :map notmuch-hello-mode-map
          "q" #'cae-notmuch-quit))

  (when (modulep! :email mu4e)
    (map! [remap compose-mail] #'+mu4e/compose))
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
                      (mu4e-sent-folder . "/Fastmail/Sent"))))))

  (when (modulep! :email mu4e +org)
    (advice-add 'mu4e~compose-handler :after #'cae-org-msg-goto-body-when-replying)))


;;; HTML

;; I don't really need LSP for the XML files I edit.
(remove-hook 'nxml-mode-local-vars-hook #'lsp!)

;;; Fennel

(after! lsp-mode
  (setf (alist-get 'fennel-mode lsp-language-id-configuration) "fennel")
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "fennel-ls")
                    :activation-fn (lsp-activate-on "fennel")
                    :server-id 'fennel-ls)))

;;; Appendix

(doom-load-packages-incrementally
 `(,@(when (modulep! :completion corfu)
       '(corfu))
   ,@(when (modulep! :emacs dirvish)
       '(dired transient dirvish))
   auth-source tramp-compat tramp-integration tramp tramp-sh
   ,@(when (modulep! :term eshell)
       '(esh-util esh-module esh-proc esh-io esh-cmd eshell
         em-tramp em-smart em-banner em-basic em-cmpl
         em-extpipe em-glob em-hist em-ls em-script em-term
         em-alias em-elecslash em-rebind em-prompt))
   ,@(when (modulep! :term vterm)
       '(vterm))
   ,@(when (modulep! :ui hydra)
       '(hydra cae-cheatsheets))
   ,@(when (or (modulep! :completion helm)
               (modulep! :private helm))
       '(async helm-lib helm-multi-match helm-source helm-core helm-global-bindings helm))
   ,@(when (modulep! :private misc-applications) ; music apps
       '(empv mpc somafm emms helm-emms))
   ,@(when (modulep! :private ai)
       '(copilot org-ai chatgpt-shell))
   ,@(when (modulep! :private gnus)
       '(gnus gnus-group gnus-sum bbdb gnus-srvr))
   ,@(when (modulep! :tools pdf)
       '(image-mode pdf-util pdf-info pdf-cache pdf-view pdf-tools))
   ,@(when (modulep! :app rss)
       '(elfeed))
   ,@(when (modulep! :app rss +org)
       '(elfeed-org))
   ,@(when (and (modulep! :completion vertico)
                (modulep! :private misc-applications))
       '(consult-gh))
   ,@(when (and (or (modulep! :completion helm)
                    (modulep! :private helm))
                (modulep! :private misc-applications))
       '(helm-system-packages helm-emms helm-linux-disks helm-rage))
   ,@(when (modulep! :private misc-applications) ; system apps
       (nconc '(trashed pulseaudio-control disk-usage daemons neato-graph-bar)
              '(snake speed-type tetris bubbles dunnet autotetris)
              '(fireplace flames-of-freedom snow zone zone-matrix zone-rainbow zone-nyan)
              '(lorem-ipsum password-generator uuidgen)
              '(leetcode)
              '(alarm pomm))))
 t)

(setq cae-config-finished-loading t)

;;Local Variables:
;;eval: (when (featurep 'aggressive-indent) (aggressive-indent-mode -1))
;;End:
