;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(load! "lisp/cae-logs")
(load! "lisp/cae-multi")
(load! "lisp/cae-keyboard")

(defvar cae-config-finished-loading nil
  "Whether the configuration has finished loading.")

;;; UI

(load! "lisp/cae-theme")

;; Do not use pagers
(setenv "PAGER" "cat")
(setenv "GIT_PAGER" "cat")

;; Set up fonts
(unless (memq system-type '(cygwin windows-nt ms-dos))
  (let ((font-size-offset (if (getenv "SSH_TTY") 0 2))) ;Different computers
    (setq doom-font (font-spec :family "Iosevka Comfy"
                               :size (+ 16 font-size-offset))
          doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo"
                                              :size (+ 16 font-size-offset))
          doom-unicode-font (unless (modulep! :ui unicode)
                              (font-spec :family "LXGW WenKai" :weight 'light
                                         :size (+ 15 font-size-offset))))))

;; Show minibuffer recursion depth
(autoload 'minibuffer-depth-setup "mb-depth")
(add-hook 'minibuffer-setup-hook  #'minibuffer-depth-setup)

;; A minimal mouse-free `tab-bar' UI.
(defadvice! cae-tab-bar-load-buttons-a ()
  :override #'tab-bar--load-buttons
  (setq tab-bar-close-button   nil
        tab-bar-back-button    nil
        tab-bar-forward-button nil
        tab-bar-new-button     nil))
(run-with-idle-timer 1.5 nil #'tab-bar-mode +1)

;; Don't confirm when exiting Emacs that active processes exist.
(setq confirm-kill-processes nil)

;; Colorize color strings.
(add-hook 'prog-mode-hook #'rainbow-mode)

(setq x-stretch-cursor t                ;Show me if I am on a TAB or a space
      truncate-string-ellipsis "..."    ;The unicode ellipsis is ugly to me
      kill-buffer-delete-auto-save-files t)

(when (and (modulep! :ui modeline)
           (not (modulep! :ui modeline +light)))
  (after! doom-modeline
    (setq doom-modeline-hud t
          doom-modeline-support-imenu t)))

(after! which-key
  (setq which-key-ellipsis "..."
        which-key-compute-remaps t))

(after! eros
  (setq eros-eval-result-prefix "⟹ "))  ;Pretty arrow

;; Do not spam me with warnings
(after! warnings
  (setq warning-minimum-level :emergency
        warning-minimum-log-level :emergency))

(after! shr
  ;; `shr' wraps lines in a visually unappealing way.
  (setq shr-width 120
        shr-max-width 120)

  ;; Sometimes EWW makes web pages unreadable by adding a bright background. Do
  ;; not colorize backgrounds at all.
  (advice-add #'shr-colorize-region :around #'ignore))

(after! proced
  (setq-default proced-auto-update-flag t))

;; Allow switching to these buffers with `C-x b'
(add-hook 'compilation-mode-hook #'doom-mark-buffer-as-real-h)
(add-hook 'debugger-mode-hook #'doom-mark-buffer-as-real-h)

(use-package! info-colors
  :defer t :init
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

(use-package! authinfo-color-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("authinfo.gpg\\'" . authinfo-color-mode))
  (advice-add 'authinfo-mode :override #'authinfo-color-mode))

(when (modulep! :ui workspaces)
  (advice-add #'which-key--process-page :around #'cae-ui-which-key-show-workspace-a))

;; Set some popup rules. How does vslot work?
(when (modulep! :ui popup)
  (set-popup-rule! "^\\*Backtrace\\*"      :size #'+popup-shrink-to-fit :quit nil :ttl nil)
  (set-popup-rule! "^\\*exwm"              :size #'+popup-shrink-to-fit :ttl nil :ttl nil)
  (set-popup-rule! "^\\*Pp Eval Output\\*" :size #'+popup-shrink-to-fit :quit nil :ttl t)
  (set-popup-rule! "^\\*org-roam\\*" :size 60 :side 'left :select nil :quit nil)
  (set-popup-rule! "^\\*info.*" :size #'cae-popup-resize-help-buffer
    :side 'right :ttl t :select t :quit t :ttl t)
  (set-popup-rule! "^\\*Man.*" :size #'cae-popup-resize-help-buffer
    :side 'right :ttl t :select t :quit t :ttl 0)
  (set-popup-rule! "^\\*tldr\\*" :size #'cae-popup-resize-help-buffer
    :side 'right :select t :quit t)
  (set-popup-rule! "^\\*helpful.*" :size #'cae-popup-resize-help-buffer
    :side 'right :select t :quit t :ttl 0)
  (set-popup-rule! "^\\*Help.*" :size #'cae-popup-resize-help-buffer
    :height 0.6 :side 'right :select t :quit t :ttl 0)
  (set-popup-rule! "^ \\*Metahelp.*" :size #'cae-popup-resize-help-buffer
    :side 'right :select t :quit t :ttl 0)
  (set-popup-rule! "^\\*Apropos.*" :size #'cae-popup-resize-help-buffer
    :height 0.6 :side 'right :select t :quit t :ttl 0)
  (set-popup-rule! "^\\*Messages\\*" :vslot -10 :height 10 :side 'bottom :select t :quit t :ttl nil)
  (after! embark
    (set-popup-rule! (regexp-quote embark--verbose-indicator-buffer) :size #'+popup-shrink-to-fit :side 'bottom :ttl t))
  (map! :map messages-buffer-mode-map :n "q" #'quit-window))

;; Lower the default popup delay.
(after! tooltip
  (setq tooltip-hide-delay 3))

(when (modulep! :checkers syntax +childframe)
  (after! flycheck-posframe
    (setq flycheck-posframe-border-width 1
          flycheck-posframe-border-use-error-face t)))

(when (and (modulep! :checkers syntax +flymake)
           (modulep! :checkers syntax +childframe))
  (defalias 'posframe-poshandler-p0p1-to-p0p1 #'posframe-poshandler-point-bottom-left-corner-upward))

(use-package! goggles
  :init
  (add-hook 'prog-mode-hook #'goggles-mode)
  (add-hook 'text-mode-hook #'goggles-mode)
  (add-hook 'conf-mode-hook #'goggles-mode)
  :config
  (setq-default goggles-pulse t))

;; Fixes an issue for me where the Vertico posframe would flicker and go blank.
(when (modulep! :completion vertico +childframe)
  (after! vertico-posframe
    (setq vertico-posframe-parameters
          '((inhibit-double-buffering . t)))))
(after! posframe
  (setq posframe-inhibit-double-buffering t))


;;; Tools

(load! "lisp/cae-webkit.el")

(setq browse-url-browser-function #'browse-url-generic
      browse-url-generic-program "chromium-bin"
      browse-url-generic-args '("--no-sandbox")
      browse-url-chromium-program "chromium-bin")

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
      history-length (expt 2 16))

(setq bookmark-bmenu-file-column 50
      bookmark-watch-bookmark-file nil)
(add-hook 'bookmark-bmenu-mode-hook #'cae-bookmark-extra-keywords)

(after! auth-source
  (setq auth-source-cache-expiry nil
        auth-sources (cl-remove-if (lambda (s) (string-suffix-p ".gpg" s))
                                   auth-sources)
        auth-source-gpg-encrypt-to nil))

(after! password-cache
  (setq password-cache-expiry nil))

(after! projectile
  ;; Automatically find projects in the I personally use.
  (setq projectile-project-search-path
        `((,doom-user-dir . 0)
          ,@(when (file-exists-p "~/projects/") '(("~/projects/" . 1)))
          ("~/src/" . 1)))
  (add-to-list 'projectile-globally-ignored-directories
               (expand-file-name ".local/straight/repos/" user-emacs-directory))
  (unless projectile-known-projects
    (projectile-discover-projects-in-search-path))
  ;; Recognize `makefile' as a Makefile.
  (cl-pushnew
   '(make marker-files
     ("makefile")
     project-file "Makefile" compilation-dir nil configure-command nil compile-command "make" test-command "make test" install-command "make install" package-command nil run-command nil)
   projectile-project-types :test #'equal)
  (cl-pushnew
   '(gnumake marker-files
     ("GNUmakefile")
     project-file "GNUMakefile" compilation-dir nil configure-command nil compile-command "make" test-command "make test" install-command "make install" package-command nil run-command nil)
   projectile-project-types :test #'equal)
  (add-to-list 'projectile-globally-ignored-directories "^.ccls-cache$")
  (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
  (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json")
  ;; Set up compilation.
  (setq projectile-per-project-compilation-buffer t
        compilation-read-command nil)
  ;; Make the project prefix more readable.
  (after! which-key
    (push '((nil . "projectile-\\(.*\\)") . (nil . "\\1"))
          which-key-replacement-alist)))

(after! tramp
  (setq tramp-terminal-prompt-regexp "[[\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*\"]"))

;; Use Emacs as the default editor for shell commands.
(define-key (current-global-map) [remap async-shell-command] 'with-editor-async-shell-command)
(define-key (current-global-map) [remap shell-command] 'with-editor-shell-command)
(add-hook 'shell-mode-hook  #'with-editor-export-editor)
(advice-add #'with-editor-export-editor :around #'cae-hacks-shut-up-a)
(add-hook 'eshell-mode-hook #'with-editor-export-editor)

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
    :defer t
    :init
    (defadvice! +pdf-view-mode-a (oldfun &rest args)
      :around #'pdf-view-mode
      (if (display-graphic-p)
          (apply oldfun args)
        (apply #'pdftotext-mode args)))))

(when (and (modulep! :tools lsp)
           (not (modulep! :tools lsp +eglot)))
  (after! lsp-mode
    (setopt lsp-headerline-breadcrumb-enable t
            lsp-enable-snippet nil
            lsp-enable-text-document-color t
            lsp-enable-folding t
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
          '("clangd" "--background-index" "--clang-tidy" "--completion-style=detailed" "--header-insertion=never" "--header-insertion-decorators=0"))))

(use-package! topsy
  :defer t :init (add-hook 'prog-mode-hook #'topsy-mode)
  :config
  ;; It's really jarring that Topsy doesn't work if the top line is a comment.
  (setf (alist-get 'rjsx-mode topsy-mode-functions) #'cae-ui-topsy-rjsx-fn))


;;; Editor

(load! "lisp/cae-repeat")

(autoload 'cae-project-bookmark (concat doom-private-dir
                                        "lisp/cae-project"))
(autoload 'cae-project-bookmark-set (concat doom-private-dir
                                            "lisp/cae-project"))
(map! :leader
      :prefix "p"
      "RET" #'cae-project-bookmark-set)
(map! :desc "project-bookmark" "C-x r p" #'cae-project-bookmark)

;; Ensure local elisp packages are up-to-date.
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'cae-compile-rebuild-package nil t)))

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; Treat all themes as safe.
(setq custom-safe-themes t)

(setq delete-selection-save-to-register "d")
(setq delete-active-region t)           ;makes `d' delete region in Meow.

(advice-add #'doom/kill-this-buffer-in-all-windows :around #'doom-set-jump-a)
(advice-add #'kill-buffer-and-window :around #'doom-set-jump-a)

;; Query buffers for a diff before killing them.
(advice-add #'kill-buffer :around #'cae-kill-buffer-a)

;; Kill buffers without asking.
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

;; Bind `tab-bar' commands consistently with the built-in keybindings.
(defadvice! cae-tab-bar-define-keys-a ()
  :after #'tab-bar--define-keys
  (unless (global-key-binding [(control f4)])
    (global-set-key [(control f4)] #'tab-close)))
(defadvice! cae-tab-bar-undefine-keys-a ()
  :after #'tab-bar--undefine-keys
  (when (eq (global-key-binding [(control f4)]) #'tab-close)
    (global-unset-key [(control f4)])))

;; Do not query before deleting a frame, since we can undo frame deletions.
(global-set-key [remap delete-frame] nil)
(global-set-key [remap delete-other-windows] #'doom/window-maximize-buffer)

;; Do not automatically continue comments.
(advice-remove #'newline-and-indent #'+default--newline-indent-and-continue-comments-a)

(map! [remap backward-kill-word] #'doom/delete-backward-word ;Do not litter the kill-ring.
      [remap upcase-word] #'upcase-dwim
      [remap downcase-word] #'downcase-dwim
      [remap capitalize-word] #'capitalize-dwim
      [remap ispell-word] #'eac-ispell-word-then-abbrev
      "C-x 4 I" #'ibuffer-other-window
      "M-Z" #'zap-up-to-char
      (:after vertico
       :map vertico-map
       "<prior>" #'vertico-scroll-down
       "<next>" #'vertico-scroll-up)
      (:after man
       :map Man-mode-map
       :n "o" #'ace-link-man)
      (:when (modulep! :config default)
       :leader
       "a" nil))

(map! :leader
      :desc "Copy link" "sy" #'link-hint-copy-link)

(when (and (not (modulep! editor evil))
           (modulep! :config default +bindings))
  (map! "C-@" #'er/expand-region
        "C-=" nil
        [remap doom/backward-to-bol-or-indent] #'beginning-of-line))

  (after! expand-region
    (setq expand-region-autocopy-register "e"
          expand-region-smart-cursor t))

(advice-add #'persp-set-keymap-prefix :override #'ignore)

(setq set-mark-command-repeat-pop t
      next-line-add-newlines t)

(setq search-whitespace-regexp ".*?"
      isearch-lax-whitespace t
      isearch-wrap-pause 'no-ding
      isearch-lazy-count t
      isearch-repeat-on-direction-change t
      isearch-allow-motion t
      isearch-allow-scroll t
      isearch-yank-on-move 'shift
      isearch-motion-changes-direction t
      lazy-count-prefix-format nil
      lazy-count-suffix-format " [%s/%s]"
      lazy-highlight-cleanup nil)
(add-hook 'doom-escape-hook
          (apply-partially #'lazy-highlight-cleanup t))

(after! ispell
  (setq ispell-quietly t
        ispell-dictionary "en_US"
        ispell-help-in-bufferp 'electric))

;; Make `smartparens' optional.
(unless (modulep! :config default +smartparens)
  (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
  (when (modulep! :term eshell)
    (after! eshell
      (remove-hook 'eshell-mode-hook #'smartparens-mode)))
  (when (modulep! :editor snippets)
    (remove-hook 'yas-before-expand-snippet-hook
                 #'+snippets--disable-smartparens-before-expand-h))

  ;; This is how we get curly braces working in C without `smartparens'.
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\{ . ?\})
                              (?\[ . ?\]))
        electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit
        electric-pair-open-newline-between-pairs t)
  (electric-pair-mode +1)
  (map! [remap newline] nil))

;; Hide commands in M-x which do not work in the current mode. Vertico commands
;; are hidden in normal buffers.
(setq read-extended-command-predicate #'command-completion-default-include-p)

(use-package! avy
  :defer t
  :init
  (map! "M-n" #'avy-goto-line-below
        "M-p" #'avy-goto-line-above
        "M-j" #'avy-goto-char-timer
        :map isearch-mode-map
        "M-j" #'avy-isearch)
  (when (modulep! :completion vertico)
    (after! vertico
      :map vertico-map
      "M-j" #'vertico-quick-jump))
  :config
  (setq avy-timeout-seconds 0.25
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
                           (ace-link-man . pre))
        avy-column-line-overlay t))

(when (modulep! :private corfu)
  (after! corfu
    (setq corfu-quick1 (cae-keyboard-kbd "asdfgh")
          corfu-quick2 (cae-keyboard-kbd "jkl;"))))

(use-package! zop-to-char
  :defer t
  :init
  (map! [remap zap-to-char] #'zop-up-to-char
        [remap zap-up-to-char] #'zop-to-char)
  :config
  (setq zop-to-char-kill-keys '(?\C-m ?\C-k ?\C-w)))

(use-package! pp+
  :after pp
  :init
  (map! [remap eval-expression] #'pp-eval-expression)
  :config
  (setq pp-read-expression-map minibuffer-local-map))

(use-package! abbrev
  :defer t :config
  (setq-default abbrev-mode t
                save-abbrevs 'silently)
  (map! :map edit-abbrevs-mode-map
        [remap save-buffer] #'abbrev-edit-save-buffer)
  (map! :map abbrev-map "e" #'edit-abbrevs))

(use-package! ibuffer
  :defer t
  :config
  (setq ibuffer-always-show-last-buffer t
        ibuffer-formats
        '((mark modified read-only locked " "
           (name 23 23 :left :elide)
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
  :defer t
  :config
  (map! :map diff-mode-map
        "q" #'kill-this-buffer))


(use-package! aggressive-indent
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode) ;See my `lisp' module.
  (add-hook 'c-mode-common-hook #'aggressive-indent-mode)
  :defer t)

;; Doom uses a custom delete backward command,
;; `+default--delete-backward-char-a', which I prefer. However, when
;; `aggressive-indent-mode' in in use, `hungry-delete-mode' works better
(use-package! hungry-delete
  :defer t :init
  (add-hook 'aggressive-indent-mode-hook #'hungry-delete-mode)
  :config
  (when (modulep! :config default +smartparens)
    (map! :map hungry-delete-mode-map
          [remap backward-delete-char-untabify] #'sp-backward-delete-char
          [remap c-electric-backspace] #'sp-backward-delete-char
          [remap delete-backward-char] #'sp-backward-delete-char
          "<deletechar>" #'sp-delete-char
          "C-d" #'sp-delete-char))
  (when (modulep! :editor multiple-cursors)
    (after! multiple-cursors
      (add-to-list 'mc--default-cmds-to-run-for-all 'hungry-delete-backward)
      (add-to-list 'mc--default-cmds-to-run-for-all 'hungry-delete-forward)))
  (add-to-list 'hungry-delete-except-modes 'eshell-mode))

;; Loading `tramp-sh' is slow, so we have this hook load auto-sudoedit if we need
;; to use sudo on a file before `tramp-sh' is loaded.
(defun cae-auto-sudoedit-maybe-h ()
  (unless (let ((path (or (buffer-file-name) list-buffers-directory)))
            (string= (file-attribute-user-id
                      (file-attributes path 'string))
                     (if (and (featurep 'tramp)
                              (tramp-tramp-file-p path))
                         (tramp-get-remote-uid (tramp-dissect-file-name path)
                                               'string)
                       (user-login-name))))
    (require 'auto-sudoedit)
    (auto-sudoedit)))
(add-hook 'find-file-hook #'cae-auto-sudoedit-maybe-h -1)
(use-package! auto-sudoedit
  :after tramp-sh
  :config
  (remove-hook 'find-file-hook #'cae-auto-sudoedit-maybe-h)
  (auto-sudoedit-mode +1))


;;; Autocompletion

(when (modulep! :private corfu)
  (load! "lisp/cae-corfu"))

(use-package! dabbrev
  :defer t :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")
        dabbrev-upcase-means-case-search t))

(use-package! hippie-exp
  :defer t :config
  (setq  hippie-expand-try-functions-list
         '(try-complete-file-name-partially
           try-complete-file-name
           try-expand-dabbrev
           try-expand-dabbrev-all-buffers
           try-expand-dabbrev-from-kill
           try-complete-lisp-symbol-partially
           try-complete-lisp-symbol
           try-expand-line)))
(map! [remap dabbrev-expand] #'hippie-expand)

(use-package! copilot
  :defer t
  :init
  (add-hook 'text-mode-hook   #'copilot-mode)
  (add-hook 'prog-mode-hook   #'copilot-mode)
  (add-hook 'conf-mode-hook   #'copilot-mode)
  :config
  (setq copilot--base-dir
        (expand-file-name ".local/straight/repos/copilot.el/" doom-emacs-dir))
  (setq copilot-node-executable (expand-file-name
                                 "~/.nvm/versions/node/v17.9.1/bin/node"))
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

  (when (modulep! :editor snippets)
    (add-hook 'yas-before-expand-snippet-hook #'copilot-clear-overlay)))

(use-package! isearch-dabbrev
  :defer t
  :init
  (map! :map isearch-mode-map
        "M-/" #'isearch-dabbrev-expand
        "C-M-/" #'isearch-dabbrev-expand))

(when (modulep! :completion vertico)
  (use-package! consult
    :init
    (map! "C-h C-m" #'describe-keymap
          "C-h <return>" #'info-emacs-manual
          "C-x C-k C-k" #'consult-kmacro ;replaces
                                        ;`kmacro-end-or-call-macro-repeat',
                                        ;which is similar to
                                        ;`kmacro-end-and-call-macro' from `<f4>'
                                        ;and `C-x e'.
          ;; C-x bindings (ctl-x-map)
          "C-x M-:" #'consult-complex-command ;orig. repeat-complex-command
          "C-x r SPC" #'consult-register-store ;orig. abbrev-prefix-mark (unrelated)
          "M-#" #'consult-register
          [remap jump-to-register] #'consult-register-load
          ;; Other custom bindings
          ;; M-g bindings (goto-map)
          "M-g e" #'consult-compile-error
          "M-g g" #'consult-goto-line   ;orig. goto-line
          "M-g M-g" #'consult-goto-line ;orig. goto-line
          "M-g o" #'consult-outline     ;Alternative: consult-org-heading
          "M-g m" #'consult-mark
          "M-g k" #'consult-global-mark
          "M-g I" #'consult-imenu-multi
          ;; M-s bindings (search-map)
          [remap Info-search] #'consult-info
          "M-s i" #'consult-info
          "M-s m" #'consult-man
          "M-s k" #'consult-keep-lines
          "M-s u" #'consult-focus-lines
          ;; Isearch integration
          "M-s e" #'consult-isearch-history
          :map isearch-mode-map
          "M-e" #'consult-isearch-history   ;orig. isearch-edit-string
          "M-s e" #'consult-isearch-history ;orig. isearch-edit-string
          "M-s l" #'consult-line       ;needed by consult-line to detect isearch
          "M-s L" #'consult-line-multi ;needed by consult-line to detect isearch
          ;; Minibuffer history
          :map minibuffer-local-map
          "M-s" #'consult-history       ;orig. next-matching-history-element
          "M-r" #'consult-history       ;orig. previous-matching-history-element
          ;; Redundant with Doom's :config default bindings
          :map global-map
          "M-g f" #'consult-flymake
          (:unless (and (modulep! :checkers syntax)
                        (not (modulep! :checkers syntax +flymake)))
           "M-g f" #'consult-flycheck)
          (:unless (modulep! :config default)
           "M-s d" #'consult-find ;does not cache files like Doom & Projectile do,
                                        ;also slower than `fd'. See Minad's comment in
                                        ;https://github.com/minad/consult/issues/363
           "M-s r" #'consult-ripgrep
           "M-s D" #'consult-locate))
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
     :preview-key 'any)))

(after! cc-mode
  (if (display-graphic-p)
      (map! :map c-mode-base-map "<tab>" #'indent-for-tab-command)
    (map! :map c-mode-base-map "TAB" #'indent-for-tab-command)))

(when (modulep! :private corfu)
  (map! (:after eshell
         :map eshell-mode-map
         "TAB" #'completion-at-point
         "<tab>" #'completion-at-point)
        (:after corfu
         :map corfu-map
         "C-M-i" #'corfu-move-to-minibuffer)
        :prefix "M-+"
        "c" #'completion-at-point       ;capf
        "t" #'complete-tag              ;etags
        "d" #'cape-dabbrev              ;or dabbrev-completion
        "f" #'cape-file
        "k" #'cape-keyword
        "h" #'cape-history
        "s" #'cape-symbol
        "a" #'cape-abbrev
        "i" #'cape-ispell
        "l" #'cape-line
        "w" #'cape-dict
        "\\" #'cape-tex
        "_" #'cape-tex
        "^" #'cape-tex
        "&" #'cape-sgml
        "r" #'cape-rfc1345
        "+" #'copilot-complete
        "M-+" #'copilot-complete))

(when (modulep! :editor snippets)
  (map! [remap yas-insert-snippet] #'consult-yasnippet
        :map yas-minor-mode-map
        "C-c & C-s" nil
        "C-c & C-n" nil
        "C-c & C-v" nil))


;;; Term

;; Enable Fish autocompletion in `read-shell-command'.
(advice-add #'shell-completion-vars :after #'fish-completion-mode)

(after! em-glob
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
        eshell-visual-subcommands '(("gh" "repo" "fork")
                                    ("git" "log" "diff" "show")
                                    ("geth" "attach"))
        eshell-visual-options '(("git" "--help" "--paginate"))))


;;; Org

(after! calendar
  (setopt calendar-week-start-day 1))

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

  (after! org-crypt
    (setq org-crypt-disable-auto-save 'encrypt))
  (after! org-agenda
    (setq org-agenda-sticky nil
          org-agenda-files '("~/org/")))
  (when (and (modulep! :ui ligatures)
             (eq (car +ligatures-in-modes) 'not))
    (add-to-list '+ligatures-in-modes 'org-mode t)))

(after! org
  ;; TODO Contribute the prefix map stuff to Org.
  (define-prefix-command 'org-babel-map)
  (define-key org-mode-map org-babel-key-prefix #'org-babel-map))

(doom-load-packages-incrementally
 `(,@(when (modulep! :private dirvish)
       '(dired transient dirvish))
   ,@'(auth-source tramp-compat tramp-integration tramp tramp-sh)
   ,@(when (modulep! :term eshell)
       '(esh-util esh-module esh-proc esh-io esh-cmd eshell
         em-tramp em-smart em-banner em-basic em-cmpl
         em-extpipe em-glob em-hist em-ls em-script em-term
         em-alias em-elecslash em-rebind em-prompt))
   ,@(when (and (modulep! :tools pdf)
                (display-graphic-p)
                (not (string-suffix-p "-WSL2" operating-system-release)))
       '(image-mode pdf-util pdf-info pdf-cache pdf-view pdf-tools)))
 t)

(setq cae-config-finished-loading t)

;;; Config
;; Here I am going over Doom's `config' module and deciding which keybindings I
;; like, etc.

;; The `C-RET' and `C-S-RET' keybindings I like but they are overriden in Org
;; mode. Meow also provides alternatives.

;; Doom should use keymap-based replacements rather than
;; `which-key-replacement-alist' for both performance reasons and so that the
;; descriptions are less sensitive to breaking, such as when we add second
;; leader key.

;;;; Files map (SPC f)

;; The recentf and sudo commands are useful enough to keep. Overall, though I
;; can use some of these commands without the leader key, these keybindings are
;; all convenient.

;; What I would be missing is mainly `recentf', sudo stuff, opening scratch
;; buffers... I think I prefer to use this as the leader key in Meow.
