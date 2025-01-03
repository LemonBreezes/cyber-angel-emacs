;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defvar cae-config-finished-loading nil)

;;; Stuff that should not be disabled.

(when cae-init-core-enabled-p
  ;; Load secrets
  (defvar cae-multi-secrets-dir (expand-file-name "secrets/" doom-user-dir))
  (make-directory cae-multi-secrets-dir t)
  (when (file-exists-p (concat cae-multi-secrets-dir "secrets.el"))
    (load! (concat cae-multi-secrets-dir "secrets.el")))

  ;; Ensure this is defined even if its module is not loaded.
  (unless (boundp '+default-minibuffer-maps)
    (defvar +default-minibuffer-maps
      (append '(minibuffer-local-map
                minibuffer-local-ns-map
                minibuffer-local-completion-map
                minibuffer-local-must-match-map
                minibuffer-local-isearch-map
                read-expression-map)
              (cond ((modulep! :completion ivy)
                     '(ivy-minibuffer-map
                       ivy-switch-buffer-map))
                    ((modulep! :completion helm)
                     '(helm-map
                       helm-rg-map
                       helm-read-file-map))))
      "A list of all the keymaps used for the minibuffer."))

  (load! "lisp/cae-tty")
  (load! "lisp/cae-bindings")
  (load! "lisp/cae-multi")              ;Run parallel Emacs instances.
  (load! "lisp/cae-smartparens")        ;Allow Smartparens to be disabled. This
                                        ;is also our Smartparens configuration.
  (load! "lisp/cae-projectile")         ;Allow Projectile to be disabled. This
                                        ;is also our Projectile configuration.
  (when (modulep! :editor evil)
    (after! evil
      (load! "lisp/cae-evil")))
  (add-hook! 'exwm-init-hook
    (load! "lisp/cae-exwm"))

  ;; Helm is not our main completion system.
  (when (and (modulep! :completion helm)
             (modulep! :completion vertico))
    (remove-hook 'doom-first-input-hook #'helm-mode))

  ;; Have a fallback completion system.
  (unless (or (modulep! :completion helm)
              (modulep! :completion ivy)
              (modulep! :completion vertico))
    (icomplete-mode +1)
    (icomplete-vertical-mode +1))

  ;; I was getting an error without this.
  (unless (modulep! :lang emacs-lisp)
    (remove-hook 'emacs-lisp-mode-hook #'overseer-enable-mode))

  (cond ((executable-find "termux-setup-storage")
         (setq xclip-method 'termux-clipboard-get)))

  ;; For some reason Persp is picking up a few buffers that it should not.
  (when (modulep! :ui workspaces)
    (add-hook! 'persp-add-buffer-on-after-change-major-mode-filter-functions
      (defun cae-persp-skip-buffer-p (buffer)
        (string-match-p "^\\*.*[lL]og.*\\*" (buffer-name buffer)))))

  ;; Set up fonts
  (unless (or (memq system-type '(cygwin windows-nt ms-dos))
              (not (cae-display-graphic-p)))
    (let ((fonts-to-check '(("Iosevka Comfy" doom-font)
                            ("IBM Plex Mono" doom-serif-font)
                            ("Iosevka Comfy Duo" doom-variable-pitch-font))))
      (dolist (font fonts-to-check)
        (if (find-font (font-spec :name (car font)))
            (set (cadr font) (font-spec :family (car font) :size 18))
          (warn "Font %s does not exist!" (car font))))))

  ;; Do not break my clipboard in SSH sessions.
  (when (and (modulep! :os tty)
             (getenv "SSH_TTY")
             (not (cae-display-graphic-p)))
    (remove-hook 'tty-setup-hook #'doom-init-clipboard-in-tty-emacs-h))

  (after! tramp
    ;; Tramp still doesn't work for me on this computer so don't blindly copy
    ;; this.
    (setq tramp-shell-prompt-pattern
          "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"
          tramp-default-remote-shell (executable-find "bash")))

  ;; Automatically mark scripts as executable.
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

  (after! ghub
    (setq ghub-use-workaround-for-emacs-bug-54989
          (version< emacs-version "29.0")))

  ;; I really don't like this advice. Just let me kill the buffer.
  (advice-remove #'kill-current-buffer #'doom--switch-to-fallback-buffer-maybe-a)

  ;; On NixOS, this is necessary.
  (when (executable-find "nixos-rebuild")
    (dolist (path '("~/.local/bin"))
      (setq path (expand-file-name path))
      (add-to-list 'exec-path path)
      (unless (member path (split-string (getenv "PATH") ":"))
        (setenv "PATH" (format "%s:%s" path (getenv "PATH"))))))

  ;; I regularly PR Doom Emacs.
  (advice-add #'doom-docs-read-only-h :override #'ignore)

  ;; If I start Emacs in `.xinitrc' without a window manager, I want to be in
  ;; fullscreen.
  (when (and (eq 'x (framep (selected-frame)))
             (not (or (getenv "EXWM_RUNNING")
                      (getenv "RATPOISON")
                      (getenv "I3SOCK")
                      (getenv "KDE_FULL_SESSION")
                      (getenv "GNOME_DESKTOP_SESSION_ID")
                      (getenv "XDG_CURRENT_DESKTOP")
                      (getenv "WAYLAND_DISPLAY")))
             (or (not (modulep! :cae exwm))
                 (bound-and-true-p cae-exwm-disabled-p)))
    (toggle-frame-fullscreen)))


;;; UI

(when cae-init-ui-enabled-p
  (load! "lisp/cae-theme")

  ;; Show absolute line numbers. I prefer to not show relative line numbers
  ;; because I use `avy' commands to jump to lines.
  (setq display-line-numbers-type t
        display-line-numbers-width-start t)

  ;; Show minibuffer recursion depth
  (autoload 'minibuffer-depth-setup "mb-depth")
  (add-hook 'minibuffer-setup-hook  #'minibuffer-depth-setup)

  ;; I don't like `hl-line-mode' globally because it sometimes conflicts with
  ;; other overlays. But in tabulated buffers like `*Proced*', it helps me see
  ;; what item I have selected.
  (add-hook 'tabulated-list-mode-hook #'hl-line-mode)

  ;; Auto-hide the tab bar.
  (after! tab-bar
    (setq tab-bar-show 1))

  ;; Don't confirm when exiting Emacs that active processes exist.
  (setq confirm-kill-processes nil)

  (setq x-stretch-cursor t              ;Show me if I am on a TAB or a space
        kill-buffer-delete-auto-save-files t
        window-combination-resize t     ;Take new window space from all other
                                        ;windows (not just current)
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
          doom-modeline-minor-modes nil))

  (after! which-key
    (setq which-key-ellipsis "..."
          which-key-idle-delay 0.5
          which-key-compute-remaps t
          which-key-max-description-length 35
          ;; For some reason opening `which-key' with a popup window open on the
          ;; same side would cause the popup window to enlarge.
          which-key-preserve-window-configuration t
          ;; This option breaks the Embark Which Key prompter when you have a
          ;; prefix key in the Embark action map so disable it.
          which-key-show-transient-maps nil))
  ;; Allow C-h to open Consult when calling which-key without a prefix.
  (when (modulep! :completion vertico)
    (after! which-key
      (setq which-key-use-C-h-commands t))
    (defvar cae-which-key-current-keymap nil)
    (defadvice! cae-which-key-update-current-keymap-a (_keymap-name keymap &rest args)
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

  ;; Do not scale fonts in `writeroom-mode'.
  (setq +zen-text-scale 0)

  (after! mule-util
    (setq truncate-string-ellipsis "...")) ;The unicode ellipsis is ugly to me

  ;; Do not spam me with warnings.
  (unless init-file-debug
    (setq warning-minimum-level :error
          warning-minimum-log-level :error))

  (after! alert
    (setq alert-default-style 'libnotify))

  (when (modulep! :ui window-select +numbers)
    (after! winum
      (setq winum-auto-setup-mode-line t))
    (add-hook 'doom-after-init-hook #'winum-mode)
    (remove-hook 'doom-switch-window-hook #'winum-mode))

  (after! shr
    ;; `shr' wraps lines in a visually unappealing way.
    (setq shr-width 120
          shr-max-width 120
          shr-use-xwidgets-for-media (featurep 'xwidget-internal))

    ;; I prefer to not use fonts in `shr' because it looks weird with the font
    ;; setup I have.
    (setq shr-use-fonts nil)

    ;; Sometimes EWW makes web pages unreadable by adding a bright background.
    ;; Do not colorize backgrounds at all.
    (advice-add #'shr-colorize-region :around #'ignore))

  ;; Allow switching to these buffers with `C-x b'
  (add-hook 'compilation-mode-hook #'doom-mark-buffer-as-real-h)

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

  ;; Set some popup rules. How does slot/vslot work? I prefer to set these popup
  ;; rules here instead of in the relevant `use-package!' blocks.
  (when (modulep! :ui popup)
    (set-popup-rules!
      ;; TODO Set the correct slot/vslot for these popups.
      '(("\\`\\*Backtrace\\*" :size +popup-shrink-to-fit :quit nil
         :ttl nil :vslot 99)
        ("\\`\\*exwm" :ignore t)
        ;;("\\`\\*lsp-ui-imenu\\*\\'" :size cae-popup-shrink-to-fit :select t :quit t
        ;; :side right :ttl 0)
        ;;("\\`\\*Ilist\\*\\'" :size cae-popup-shrink-to-fit :select t :quit t
        ;; :side right :ttl 0)
        ("\\`\\*difftastic git diff\\*\\'" :size +popup-shrink-to-fit
         :select t :quit t :side bottom :ttl 0)
        ("\\`\\*Pp Eval Output\\*" :size +popup-shrink-to-fit
         :quit t :ttl t)
        ("\\`\\*org-roam\\*" :size 60 :side left :select nil
         :quit nil)
        ("\\`\\*info.*" :size cae-popup-resize-help-buffer
         :side right :ttl nil :select t :quit t :ttl t :slot 2 :vslot 2)
        ("\\`\\*\\(?:Wo\\)?Man " :size cae-popup-resize-help-buffer
         :side right :ttl t :select t :quit t :ttl 0 :vslot -6)
        ("\\`\\*tldr\\*" :size cae-popup-resize-help-buffer
         :side right :select t :quit t)
        ("\\`\\*Diff\\*" :size cae-popup-resize-help-buffer
         :side right :select t :quit t :ttl 0)
        ("\\`\\*Ibuffer Diff\\*" :size cae-popup-resize-help-buffer
         :side right :select t :quit t :ttl 0)
        ("\\`\\*\\([Hh]elp\\|Apropos\\)"
         :size cae-popup-resize-help-buffer :side right :select t :quit t :ttl 0
         :slot 2 :vslot -8)
        ("\\` \\*Metahelp.*" :size cae-popup-resize-help-buffer
         :side right :select t :quit t :ttl 0 :slot 2 :vslot -9)
        ("\\`\\*Messages\\*" :vslot -10 :height 10 :side bottom
         :select t :quit t :ttl nil :vslot 99)
        ("\\`\\*eww.*" :size cae-popup-resize-help-buffer :side right
         :select t :ttl nil)
        ("\\`\\*w3m\\*\\'" :size cae-popup-resize-help-buffer
         :side right :select t :ttl nil)
        ("\\`\\*dap-ui-repl\\*\\'" :vslot -5 :size 0.3 :select t
         :modeline nil :quit nil :ttl nil)
        ("\\`SpeedRect Command Key Help\\'" :size cae-popup-resize-help-buffer
         :side right :select nil :quit t :ttl 0)
        ("\\`\\*ednc-log\\*\\'" :size cae-popup-resize-help-buffer
         :side right :select nil :quit t :ttl nil)
        ("*Notification [0-9]+" :side top :size +popup-shrink-to-fit
         :select nil)
        ("\\`\\*tldr\\*\\'" :size cae-popup-resize-help-buffer
         :side right :ttl t :select t :quit t :ttl 0)
        ("\\`\\*Shortdoc .*" :size cae-popup-resize-help-buffer
         :side right :ttl t :select t :quit t :ttl 0)
        ;;("\\`\\*ielm\\*\\'" :size cae-popup-resize-help-buffer
        ;; :side right :ttl nil :select t :quit t :ttl 0)
        ("\\`\\*devdocs\\*\\'" :width 122
         :side right :ttl t :select t :quit t :ttl 0)
        ("\\`Trash Can" :size 0.3 :side bottom :select t :quit t
         :ttl 0)
        ("\\`\\*evil-owl\\*\\'" :side bottom :select nil :ttl 0
         :size cae-popup-shrink-to-fit)
        ("\\`\\*chatgpt\\* " :size 0.3 :select t :quit nil :ttl nil)
        ("\\`\\*dall-e\\*.*" :size 0.3 :select t :quit nil :ttl nil)
        ("\\`\\*edit-indirect " :side top :select t :ttl 0 :size cae-popup-shrink-to-fit)
        ("\\`\\*vterm" :quit nil :ttl nil :size 0.3)
        ("\\`\\*notmuch-hello"  :ignore)
        ("\\`\\*gud-" :ttl nil :size 0.35)
        ("embrace-help" :side top :size +popup-shrink-to-fit)
        ("*helm " :ignore t)
        ("\\`\\*Async Shell Command\\*\\'" :side top :select nil :ttl 0 :quit t
         :size cae-popup-shrink-to-fit)
        ("*Neato Graph Bar" :side top :quit t :ttl 0 :size
         (lambda (win) (set-window-text-height win (+ (num-processors) 2))))))
    (after! embark
      (set-popup-rule! (regexp-quote embark--verbose-indicator-buffer)
        :size #'+popup-shrink-to-fit :side 'bottom :ttl t)
      (set-popup-rule! "\\`\\*Embark Export: "
        :size #'cae-popup-resize-help-buffer :side 'right :ttl 0))
    (after! elfeed
      (set-popup-rule! (format "\\`%s\\'" (regexp-quote elfeed-log-buffer-name))
        :size '+popup-shrink-to-fit :side 'right :select nil :quit t :ttl nil))
    (after! bbdb
      (set-popup-rule! (format "\\`%s\\'" (regexp-quote bbdb-buffer-name))
        :select nil :quit t :ttl nil))
    (map! :map messages-buffer-mode-map :n "q" #'quit-window))

  (when (modulep! :ui workspaces)
    (defadvice! cae-which-key-show-workspace (orig-fun &rest pages-obj)
      "Show my workspaces in the echo area."
      :around #'which-key--process-page
      (let ((out (apply orig-fun pages-obj)))
        (if (not (string-match-p "Display tab bar\\b" (car out)))
            out
          (cons (car out)
                (lambda ()
                  (funcall (cdr out))
                  (which-key--echo (concat (current-message) " "
                                           (+workspace--tabline))))))))

    (defadvice! cae-open-journal-in-new-workspace (orig-fun &rest args)
      "Open the journal in a new workspace."
      :before #'org-journal-new-entry
      (+workspace-switch "journal" t)))

  ;; Lower the default popup delay.
  (after! tooltip
    (setq tooltip-hide-delay 3))

  (after! flycheck-posframe
    (setq flycheck-posframe-border-width 1
          flycheck-posframe-border-use-error-face t))

  ;;(use-package! breadcrumb
  ;;  :unless (modulep! :ui modeline)
  ;;  :defer t :init
  ;;  (add-hook 'emacs-lisp-mode-hook #'breadcrumb-local-mode))

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
    (add-hook! 'persp-activated-functions
      (defun cae-beacon-blink-on-workspace-switch (&rest _)
        (run-at-time 0.01 nil #'beacon-blink-automated)))
    (add-hook 'persp-created-functions #'cae-beacon-blink-on-workspace-switch)
    (after! corfu
      (add-hook 'beacon-dont-blink-predicates #'cae-corfu-visible-p))
    (setq beacon-blink-when-window-scrolls nil
          beacon-overlay-priority -1)
    (dolist (cmd '(+eshell-tldr-to-man))
      (add-to-list 'beacon-dont-blink-commands cmd)))

  (use-package indent-bars
    :custom
    (indent-bars-treesit-support t)
    (indent-bars-no-descend-string t)
    (indent-bars-treesit-ignore-blank-lines-types '("module"))
    (indent-bars-treesit-wrap '((python argument_list parameters ; for python,
                                        ; as an example
                                 list list_comprehension
                                 dictionary dictionary_comprehension
                                 parenthesized_expression subscript)))
    :hook ((python-base-mode yaml-mode lua-mode) . indent-bars-mode))

  (use-package! casual-calc
    :defer t :init
    (after! calc
      (map! :map calc-mode-map
            "C-o" #'casual-main-menu
            "C-M-?"#'casual-main-menu))))


;;; Tools

(when cae-init-tools-enabled-p
  (when (modulep! :tools lsp)
    (load! "lisp/cae-lsp")
    (load! "lisp/cae-semantic"))

  ;; I use `w3m' because EWW is too slow.
  (use-package! w3m
    :defer t :init
    (setq browse-url-secondary-browser-function #'w3m-browse-url)
    :config
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
    (setq browse-url-secondary-browser-function
          (if (eq browse-url-secondary-browser-function #'browse-url-default-browser)
              #'eww-browse-url
            browse-url-secondary-browser-function)
          browse-url-browser-function
          (cond ((executable-find "termux-setup-storage")
                 #'browse-url-xdg-open)
                ((getenv "SSH_TTY") browse-url-secondary-browser-function)
                (t #'browse-url-generic))
          browse-url-firefox-new-window-is-tab t)

    (defvar cae-generic-browser-name nil)
    (cond
     ((getenv "WSL_DISTRO_NAME")
      (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
            browse-url-generic-args '("/c" "start")))
     ((executable-find "firefox-beta")
      (setq browse-url-generic-program "firefox-beta"
            browse-url-generic-args '("--new-tab")
            cae-generic-browser-name "Firefox"))
     ((executable-find "firefox")
      (setq browse-url-generic-program "firefox"
            browse-url-generic-args '("--new-tab")
            cae-generic-browser-name "Firefox"))
     ((executable-find "firefox-bin")
      (setq browse-url-generic-program "firefox-bin"
            browse-url-generic-args '("--new-tab")
            cae-generic-browser-name "Firefox"))
     ((when-let* ((chrome (or (executable-find "chromium-bin-browser")
                              (executable-find "google-chrome-unstable")
                              (executable-find "google-chrome-stable"))))
        (setq browse-url-generic-program chrome
              browse-url-generic-args (when (eq (user-uid) 0)
                                        '("--no-sandbox"))
              cae-generic-browser-name "Chrome")))))

  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-to-list 'doom-large-file-excluded-modes 'nov-mode)

  (add-to-list 'auto-mode-alist '("/sway/.*config.*/" . i3wm-config-mode))
  (add-to-list 'auto-mode-alist '("/sway/config\\'" . i3wm-config-mode))

  ;; Set up printers.
  (after! lpr (setq printer-name "Brother_HL-2350DW"))
  (after! ps-print (setq ps-printer-name "Brother_HL-2350DW"))
  (after! pdf-misc
    (setq pdf-misc-print-program-executable (executable-find "lpr")))

  ;; Was reading this
  ;; https://github.com/link0ff/emacs-init/blob/1fc141e20092cc357f2c6021626635e8ac067b8c/emacs.custom.el.
  (setq delete-by-moving-to-trash t
        ;; Careful with these settings! They can prevent errors and Emacs
        ;; freezing but they can also surprise you if you expect all your files
        ;; to be trashed when you delete them.
        remote-file-name-inhibit-delete-by-moving-to-trash t
        remote-file-name-inhibit-auto-save t
        remote-file-name-inhibit-auto-save-visited t
        ;; ---
        large-file-warning-threshold 100000000 ; ~100 MB
        yank-pop-change-selection t
        global-mark-ring-max 1024
        history-delete-duplicates t
        history-length t
        mark-ring-max 1024
        message-log-max t
        kill-ring-max 1024
        kill-whole-line t
        list-matching-lines-jump-to-current-line t
        mouse-prefer-closest-glyph t
        next-error-message-highlight 'keep
        read-char-by-name-sort 'code
        revert-buffer-quick-short-answers t
        scroll-error-top-bottom t
        scroll-preserve-screen-position t
        shift-select-mode 'permanent
        track-eol t
        visual-order-cursor-movement t
        what-cursor-show-names t)

  ;; https://idiomdrottning.org/show-trailing-whitespace
  ;; `show-trailing-whitespace' is my friend.
  (setq-hook! (text-mode prog-mode conf-mode)
    show-trailing-whitespace t)

  (after! cus-edit
    (setq custom-buffer-done-kill t))

  (after! saveplace
    (setq save-place-limit nil))

  (after! grep
    (setq grep-use-headings t
          grep-program "rg"))

  (after! tar-mode
    (setq tar-mode-show-date t))

  (after! info
    (setq Info-fontify-maximum-menu-size t))

  (after! descr-text
    (setq describe-char-unicodedata-file
          (concat cae-multi-data-dir "UnicodeData.txt")))

  (after! smiley
    (setq smiley-style t))

  (after! xref
    (setq xref-search-program 'ripgrep))

  ;; We use `corfu' and `vertico' instead of the built-in completions, but I
  ;; still have this here as my preferred defaults for the built-in completions.
  (setq completion-auto-select 'second-tab
        completions-detailed t
        completions-format 'vertical
        completions-group t
        completions-group-sort 'alphabetical)

  ;; These are not used in Doom Emacs since we use `helpful' instead, but I have
  ;; them here as "better defaults" so-to-say.
  (add-hook 'help-fns-describe-function-functions
            #'shortdoc-help-fns-examples-function)
  (after! help
    (setq help-enable-symbol-autoload t
          help-enable-completion-autoload t
          help-enable-symbol-autoload t
          help-window-select t
          help-clean-buttons t
          help-enable-variable-value-editing t))

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
    ;; The `first-error' value stops scrolling at the first warning too, which I
    ;; don't like.
    (setq compilation-scroll-output t)
    ;; Testing to see if this fixes some issues with compilation buffer escape
    ;; sequences not being colorized.
    (remove-hook 'compilation-filter-hook #'comint-truncate-buffer))

  (after! tramp
    (setq tramp-use-scp-direct-remote-copying t
          tramp-allow-unsafe-temporary-files t)
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
    (let* ((posframe-supported-p (and (modulep! :completion vertico +childframe)
                                      (or (cae-display-graphic-p)
                                          (> emacs-major-version 30))))
           (wide-frame-p (>= (frame-width) 120))
           (default-view (if wide-frame-p
                             (if posframe-supported-p 'posframe 'vertical)
                           'buffer))
           (new-entries `((embark-keybinding . (, (if wide-frame-p 'grid 'buffer)))
                          (consult-grep . (, (if wide-frame-p
                                                 (if posframe-supported-p 'posframe 'buffer)
                                               'buffer)))
                          (imenu . (, (if wide-frame-p
                                          (if posframe-supported-p '(posframe grid) '(grid))
                                        'buffer)))
                          (consult-location . (, (if wide-frame-p
                                                     (if posframe-supported-p 'posframe 'buffer)
                                                   'buffer)))
                          (file . (, default-view))
                          (t . (, default-view)))))
      (dolist (entry new-entries)
        (let ((key (car entry))
              (value (cdr entry)))
          ;; Merge the new value with existing values, ensuring idempotency
          (setf (alist-get key vertico-multiform-categories)
                (cl-delete-duplicates
                 (append (alist-get key vertico-multiform-categories)
                         value)
                 :test #'equal))))
      ;; The order matters. We reverse to give priority to earlier entries.
      (setq vertico-multiform-categories (nreverse vertico-multiform-categories))
      ;; Ensure `execute-extended-command' has the correct view
      (let* ((execute-command-view default-view)
             (existing-views (alist-get 'execute-extended-command vertico-multiform-commands))
             (cleaned-views (cl-set-difference existing-views
                                               '(flat vertical posframe grid reverse unobtrusive))))
        (setf (alist-get 'execute-extended-command vertico-multiform-commands)
              (cl-delete-duplicates
               (cons execute-command-view cleaned-views)
               :test #'equal)))))

  ;; Use Emacs as the default editor for shell commands.
  (when (cae-display-graphic-p)
    (dolist (hook '(shell-mode-hook eshell-mode-hook vterm-mode-hook))
      (dolist (fn '(with-editor-export-editor
                    with-editor-export-hg-editor
                    with-editor-export-git-editor))
        (add-hook hook fn)))
    (advice-add #'with-editor-export-editor :around #'cae-shut-up-a))

  (after! spell-fu
    (add-to-list 'spell-fu-faces-exclude 'message-header-other)
    (add-to-list 'spell-fu-faces-exclude 'org-property-value)
    (add-to-list 'spell-fu-faces-exclude 'message-header-to)
    (setq spell-fu-faces-exclude
          (delq 'font-lock-string-face spell-fu-faces-include)))

  (after! spell-fu
    (add-hook! 'nxml-mode-hook
      (defun cae-disable-spell-fu-h ()
        (spell-fu-mode -1))))

  (use-package! 0x0
    :defer t :init
    (map! "C-x U" #'0x0-dwim)
    (after! embark
      (define-key embark-region-map (kbd "U") '0x0-dwim)))

  (after! consult-yasnippet
    (setq consult-yasnippet-use-thing-at-point t
          consult-yasnippet-always-overwrite-thing-at-point t)))


;;; Editor

(when cae-init-editor-enabled-p
  (load! "lisp/cae-repeat")
  (load! "lisp/cae-vlf")
  (load! "lisp/cae-restore-point")
  ;;(load! "lisp/cae-visible-mark")
  (load! "lisp/cae-visible-scrolling")

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
  (when (fboundp #'kill-ring-deindent-mode)
    (add-hook 'doom-first-input-hook #'kill-ring-deindent-mode))

  (after! paren
    (setq show-paren-context-when-offscreen 'overlay
          show-paren-ring-bell-on-mismatch t))

  (advice-add #'doom/kill-this-buffer-in-all-windows :around #'doom-set-jump-a)
  (advice-add #'kill-this-buffer :around #'doom-set-jump-a)

  (setq find-sibling-rules
        '(("\\([^/]+\\)\\.org\\'" "~/org/.archive/\\\\1.org\\'")))

  ;; Query buffers for a diff before killing them.
  (defvar cae-diff-window nil
    "Variable to store the diff window created by 'cae-ask-kill-buffer'.")
  (defun cae-ask-kill-buffer ()
    "Ask to diff, save or kill buffer"
    (if (and (buffer-file-name)
             (buffer-modified-p))
        (prog1
            (cl-loop
             for ch =
             (read-key "(k)ill buffer, (d)iff buffer, (s)ave buffer, (q)uit?")
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

  ;; Kill process buffers without asking.
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
        ;; The default search ring size is 16, which is too small considering
        ;; that we can fuzzy search the history with Consult.
        search-ring-max 200
        regexp-search-ring-max 200)
  (add-hook! 'doom-escape-hook :depth -1
    (defun cae-clean-up-lazy-highlight-h ()
      (lazy-highlight-cleanup t)))
  (when (fboundp #'+evil-disable-ex-highlights-h)
    (add-hook 'doom-escape-hook #'+evil-disable-ex-highlights-h -1))

  ;; Autokill buffers which have not been displayed for 3 days.
  (run-with-idle-timer 30 nil #'midnight-mode +1)
  (after! midnight
    (setq clean-buffer-list-kill-regexps '("\\`\\*.*\\*\\'")
          clean-buffer-list-delay-special 7200)
    (add-to-list 'clean-buffer-list-kill-never-buffer-names
                 doom-fallback-buffer-name))

  (after! outline
    (setq outline-minor-mode-use-buttons t))

  (after! ispell
    (setq ispell-quietly t
          ispell-dictionary "en_US"
          ispell-help-in-bufferp 'electric)
    (when-let* ((nixos-aspell-dir "/run/current-system/sw/lib/aspell")
                (_ (file-exists-p nixos-aspell-dir)))
      (setq ispell-aspell-data-dir nixos-aspell-dir
            ispell-aspell-dict-dir nixos-aspell-dir)))

  (after! vline
    (setq vline-idle-time 0.1))

  (after! undo-fu
    (setq undo-fu-allow-undo-in-region t))

  ;; Hide commands in M-x which do not work in the current mode. Vertico
  ;; commands are hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  (after! embark
    (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target))

  (use-package! avy
    :defer t :init
    (defadvice! cae-avy-use-post-style-a (oldfun &rest args)
      :around #'avy-goto-end-of-line
      (let ((avy-style 'post))
        (apply oldfun args)))
    :config
    (setq avy-timeout-seconds 0.4
          avy-all-windows t
          avy-keys (cae-keyboard-remap
                    '(?a ?s ?d ?f ?g
                      ;; Removed ?\; because it's not on default Android
                      ;; touchscreen keyboards.
                      ?h ?j ?k ?l))
          avy-background nil
          avy-single-candidate-jump t
          avy-escape-chars '(?\C-g ?\C-\[ ?\s)
          avy-dispatch-alist
          (cae-keyboard-remap
           '((?x . avy-action-kill-move) (?X . avy-action-kill-stay)
             (?t . avy-action-teleport) (?m . avy-action-mark) (?n . avy-action-copy)
             (?y . avy-action-yank) (?Y . avy-action-yank-line) (?i . avy-action-ispell)
             (?z . avy-action-zap-to-char)))
          avy-styles-alist '((avy-isearch . pre)
                             (ace-link-man . pre)
                             (avy-goto-end-of-line . post)
                             (avy-kill-ring-save-region . pre)
                             (avy-kill-region . pre)
                             (avy-copy-region . pre)
                             (avy-move-region . pre))))

  (use-package! embrace
    :defer t :init
    ;; Respect popup rules with the Embrace help popup.
    (defadvice! +cae-embrace-use-popup-a (oldfun help-string)
      :around #'embrace--show-help-buffer
      (cl-letf (((symbol-function #'display-buffer-in-side-window)
                 (symbol-function #'display-buffer)))
        (funcall oldfun help-string)))
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
    (setq ibuffer-always-show-last-buffer t)
    (after! ibuffer-ext
      (add-to-list 'ibuffer-never-show-predicates #'doom-unreal-buffer-p)))

  (use-package! yank-indent
    :defer t :init (add-hook 'doom-first-buffer-hook #'global-yank-indent-mode)
    :config
    (advice-add #'cae-yank-indent-a :after #'yank-indent--after-yank-advice))

  (use-package! file-info
    :defer t :init
    (map! :leader :desc "Show file info" "fi" #'file-info-show)
    :config
    ;; See the `:cae vc' module for further configuration.
    (setq file-info-include-headlines t
          file-info-max-value-length 100))

  (use-package! keyfreq
    :after-call post-command-hook
    :config
    (keyfreq-mode +1)
    (keyfreq-autosave-mode +1))

  (use-package! wakatime-mode
    :defer t :init
    (add-hook 'doom-first-file-hook #'global-wakatime-mode)
    :config
    (setq wakatime-cli-path (executable-find "wakatime"))
    (defadvice! cae-wakatime-handle-remote-files-a (_)
      :before-until #'wakatime-call
      (tramp-tramp-file-p (buffer-file-name (current-buffer)))))

  (use-package! titlecase
    :defer t :init
    (after! embark
      (define-key embark-region-map "T" #'titlecase-region)
      (define-key embark-heading-map "T" #'titlecase-line)
      (define-key embark-sentence-map "T" #'titlecase-sentence)))

  (after! outline
    (after! which-key
      (which-key-add-keymap-based-replacements outline-minor-mode-map
        "C-c @" "outline")))

  (use-package! embark
    :defer t :config
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
    (aas-set-snippets 'global
      ";--" "—"
      ";-." "→"
      ";=." "⇒"
      ";!=" "≠"
      ",." #'cae-insert-bracket-pair ; <>
      "-0" "->"
      "0-" "<-"
      "=0" "=>"
      "0=" "<="))

  (use-package! expand-region-improved
    :defer t :config
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
    :when (modulep! :tools magit)
    :defer t :init
    ;; Wrangle parrot into being fully lazy-loaded.
    (autoload #'parrot-party-while-process "parrot")
    (autoload #'parrot--todo-party "parrot")
    (autoload #'parrot--magit-push-filter "parrot")
    (defadvice! cae-modeline-gac-party-on-push-a (buffer)
      :after #'gac-push
      (when-let* ((proc (get-buffer-process "*git-auto-push*")))
        (parrot-party-while-process proc)))
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
    (map! :n "]r" #'cae-modeline-rotate-forward-word-at-point
          :n "[r" #'cae-modeline-rotate-backward-word-at-point)
    :config
    (after! parrot-rotate
      (setq parrot-rotate-animate-after-rotation t
            parrot-rotate-highlight-after-rotation t
            parrot-rotate-start-bound-regexp "[\]\[[:space:](){}<>]"
            parrot-rotate-end-bound-regexp "[\]\[[:space:](){}<>]")
      (add-to-list 'parrot-rotate-dict '(:rot ("add-hook" "remove-hook")))
      (add-to-list 'parrot-rotate-dict '(:rot ("add-hook!" "remove-hook!")))
      (add-to-list 'parrot-rotate-dict '(:rot ("Yes" "No")))
      (add-to-list 'parrot-rotate-dict '(:rot ("nil" "t")))
      (add-to-list 'parrot-rotate-dict '(:rot ("when" "unless")))
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
    (defvaralias 'naming-convention-map 'doom-leader-naming\ convention-map)
    (after! evil
      (evil-define-operator evil-operator-string-inflection (beg end _type)
        "Define a new evil operator that cycles symbol casing."
        :move-point nil
        (interactive "<R>")
        (string-inflection-all-cycle)
        (setq evil-repeat-info '([?g ?~])))
      (define-key evil-normal-state-map (kbd "g~")
        'evil-operator-string-inflection)))

  (use-package! beginend
    :defer t :init
    (add-hook 'doom-first-input-hook #'beginend-global-mode)
    ;; This patches around this function not being compatible with Evil when
    ;; `evil-move-beyond-eol' is `nil'. This should probably go into
    ;; `evil-collection'.
    (defadvice! cae-beginend-goto-eol-a ()
      :before #'beginend-prog-mode-goto-end
      (goto-char (point-at-eol))))

  (use-package! isearch-mb
    :after-call isearch-mode-hook :init
    (when (modulep! :editor evil)
      (after! evil
        ;; I prefer `isearch' combined with `isearch-mb'.
        (setq! evil-search-module 'isearch)
        (when evil-want-C-w-delete
          (map! :map isearch-mb-minibuffer-map
                "C-w" #'evil-delete-backward-word))))
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
    (define-key isearch-mb-minibuffer-map (kbd "M-j") #'avy-isearch)
    (when (modulep! :completion vertico)
      (map! :map isearch-mb-minibuffer-map
            [remap consult-history] #'consult-isearch-history)
      (define-key isearch-mb-minibuffer-map (kbd "M-s l") 'consult-line))
    (map! :map isearch-mb-minibuffer-map
          [remap isearch-query-replace] #'anzu-isearch-query-replace
          [remap isearch-query-replace-regexp] #'anzu-query-replace-regexp))

  (use-package! edit-indirect
    :defer t :config
    (add-hook! 'edit-indirect-after-creation-hook
      (defun cae-edit-indirect-major-mode-fallback-h ()
        (when (eq major-mode 'fundamental-mode)
          (funcall (buffer-local-value
                    'major-mode
                    (overlay-buffer edit-indirect--overlay))))))
    (add-hook! 'edit-indirect-after-creation-hook :append
      (defun cae-edit-indirect-setup-defaults-h ()
        (when (bound-and-true-p flycheck-mode)
          (flycheck-mode -1)))))

  (use-package! zop-to-char
    :defer t :init
    (map! [remap zap-to-char] #'zop-to-char
          [remap zap-up-to-char] #'zop-up-to-char)
    :config
    (setq zop-to-char-kill-keys '(?\C-m ?\C-k ?\C-w))))


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
  (after! em-glob
    ;; Allow us to type HEAD~1, HEAD~2, etc., as arguments to git commands.
    (setq eshell-error-if-no-glob nil))

  (after! vterm
    (setq vterm-max-scrollback 100000))

  (after! comint
    (setq comint-history-isearch 'dwim
          comint-buffer-maximum-size 8192))

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
                                      ,@(unless (string= (getenv "GIT_PAGER")
                                                         "cat")
                                          '(("git" "log" "diff" "show"))))
          eshell-visual-options '(("git" "--help" "--paginate")))))


;;; Text

(when cae-init-text-enabled-p
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'auto-fill-mode)

  (after! calendar
    (setq calendar-week-start-day 1
          calendar-mark-diary-entries-flag t))

  (after! org
    (setq org-directory "~/org/"
          org-extend-today-until 3
          org-startup-with-inline-images t
          org-startup-align-all-tables t
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
          '((?A . nerd-icons-red)
            (?B . nerd-icons-orange)
            (?C . nerd-icons-yellow)
            (?D . nerd-icons-green)
            (?E . nerd-icons-blue))
          ;; All my computers use 64-bit processors
          org-read-date-force-compatible-dates nil)
    (when (modulep! :lang org +roam2)
      (setq +org-roam-auto-backlinks-buffer nil))
    (map! :map org-mode-map
          "C-c C-M-h" #'er/mark-org-code-block)
    (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
    (after! org-list
      (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")
                                            ("1." . "a."))))
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
        "C-c C-v" "org-babel-map")))

  (after! org-journal
    (setq org-journal-file-format "%Y-%m-%d.org"))
  (when (modulep! :editor evil)
    (add-hook 'org-journal-after-entry-create-hook #'evil-insert-state))

  (after! markdown-mode
    (setq markdown-fontify-code-blocks-natively t)))


;;; Email

;; Always enable the essential email configuration, including maildirs and addresses.
;; This is crucial for testing and debugging any issues with the email setup.
(setq user-full-name "StrawberryTea"
      user-mail-address "look@strawberrytea.xyz"
      mail-host-address "strawberrytea.xyz"
      mail-source-directory "~/.mail/")

(after! mu4e
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
  (setq report-emacs-bug-no-explanations t)
  (autoload 'async-smtpmail-send-it "smtpmail-async" nil t)
  (setq compose-mail-user-agent-warnings nil)
  (after! sendmail
    (setq send-mail-function #'async-smtpmail-send-it
          mail-self-blind t))
  (after! message
    (setq message-send-mail-function #'async-smtpmail-send-it
          message-forward-as-mime t
          message-forward-before-signature t)
    (add-hook 'message-setup-hook #'message-check-recipients))

  (setq +notmuch-home-function (lambda () (notmuch-search "tag:inbox")))
  (after! notmuch
    (map! :map notmuch-search-mode-map
          "q" #'cae-notmuch-quit))
  (after! notmuch-hello
    (map! :map notmuch-hello-mode-map
          "q" #'cae-notmuch-quit))

  (when (modulep! :email mu4e)
    (use-package! consult-mu
      :when (and (modulep! :email mu4e)
                 (modulep! :completion vertico))
      :commands consult-mu
      :defer t :custom
      (consult-mu-maxnum 200)
      (consult-mu-preview-key 'any)
      (consult-mu-mark-previewed-as-read nil)
      (consult-mu-mark-viewed-as-read t)
      (consult-mu-use-wide-reply t)
      ;; define a template for headers view in minibuffer. The example below adjusts the width based on the width of the screen.
      (consult-mu-headers-template (lambda () (concat "%f" (number-to-string (floor (* (frame-width) 0.15))) "%s" (number-to-string (floor (* (frame-width) 0.5))) "%d13" "%g" "%x")))
      :init
      (map! [remap mu4e-search] #'consult-mu)
      :config
      ;;create a list of saved searches for quick access using `history-next-element' with `M-n' in minibuffer. Note the "#" character at the beginning of each query! Change these according to
      (setq consult-mu-saved-searches-dynamics '("#flag:unread"))
      (setq consult-mu-saved-searches-async '("#flag:unread"))
      ;; require embark actions for marking, replying, forwarding, etc. directly from minibuffer
      (require 'consult-mu-embark)
      ;; require extra module for composing (e.g. for interactive attachment) as well as embark actions
      (require 'consult-mu-compose)
      (require 'consult-mu-compose-embark)
      ;; require extra module for searching contacts and runing embark actions on contacts
      (require 'consult-mu-contacts)
      (require 'consult-mu-contacts-embark)
      (setq consult-mu-embark-attach-file-key "C-a")
      (setq consult-mu-contacts-ignore-list '("^.*no.*reply.*"))
      (setq consult-mu-contacts-ignore-case-fold-search t)
      (consult-mu-compose-embark-bind-attach-file-key)
      ;; choose if you want to use dired for attaching files (choice of 'always, 'in-dired, or nil)
      (setq consult-mu-compose-use-dired-attachment 'in-dired)
      ;; `consult-mu-dynamic' is way too slow.
      (setq consult-mu-default-command #'consult-mu-async))

    (map! [remap compose-mail] #'+mu4e/compose))
  (after! mu4e-vars
    (setq mu4e-modeline-support t
          mu4e-notification-support t)))


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
                      (assoc-delete-all "comment" lsp-semantic-token-faces)))))))

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
          "G" #'consult-hoogle-project)))

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
       '(hydra))
   ,@(when (modulep! :email mu4e)
       '(mu4e))
   ,@(when (or (modulep! :completion helm)
               (modulep! :cae helm))
       '(async helm-lib helm-multi-match helm-source helm-core
         helm-global-bindings helm))
   ,@(when (modulep! :cae misc-applications) ; music apps
       '(empv mpc somafm emms helm-emms))
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
   ,@(when (modulep! :app rss)
       '(elfeed))
   ,@(when (modulep! :app rss +org)
       '(elfeed-org))
   ,@(when (and (modulep! :completion vertico)
                (modulep! :cae misc-applications))
       '(consult-gh))
   ,@(when (and (modulep! :completion vertico)
                (modulep! :email mu4e))
       '(consult-mu))
   ,@(when (and (or (modulep! :completion helm)
                    (modulep! :cae helm))
                (modulep! :cae misc-applications))
       '(helm-system-packages helm-emms helm-linux-disks helm-rage))
   ,@(when (modulep! :cae misc-applications) ; system apps
       (nconc '(trashed pulseaudio-control disk-usage daemons neato-graph-bar)
              '(snake speed-type tetris bubbles dunnet autotetris klondike)
              '(fireplace flames-of-freedom snow zone zone-matrix zone-rainbow
                zone-nyan selectric)
              '(lorem-ipsum password-generator uuidgen)
              '(leetcode)
              '(alarm pomm debbugs)))
   ,@(when (modulep! :editor multiple-cursors)
       (if (modulep! :editor evil)
           '(evil-multiedit evil-mc)
         '(multiple-cursors)))
   ,@(when (modulep! :editor format)
       '(apheleia)))
 t)

(unless (executable-find "termux-setup-storage")
  (after! pdf-tools
    (pdf-tools-install t nil t)))

(after! copilot
  (unless (or (not copilot-node-executable)
              (file-exists-p copilot-install-dir))
    (copilot-install-server)))

(setq cae-config-finished-loading t)

;;Local Variables:
;;eval: (when (featurep 'aggressive-indent) (aggressive-indent-mode -1))
;;End:
