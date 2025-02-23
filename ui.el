;;; ui.el -*- lexical-binding: t; -*-

(load! "lisp/cae-theme")
(load! "lisp/cae-visual-scrolling")

;; Show absolute line numbers. I prefer to not show relative line numbers
;; because I use `avy' commands to jump to lines.
(setq display-line-numbers-type nil
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

;; See https://github.com/oantolin/embark/issues/470, having this value too low
;; causes keys to "hang around" in Embark after performing actions.
(setq echo-keystrokes 1)
(after! embark
  (setq embark-confirm-act-all nil))
(after! which-key
  (setq which-key-ellipsis "..."
        which-key-idle-delay 0.5
        which-key-compute-remaps t
        which-key-max-description-length 35
        ;; This is so that `which-key' does not cause popup shells to be
        ;; resized unintentionally.
        which-key-preserve-window-configuration t
        ;; This option breaks the Embark Which Key prompter when you have a
        ;; prefix key in the Embark action map so disable it.
        which-key-show-transient-maps nil))
(when (modulep! :editor evil)
  (after! evil
    ;; I have gotten a strange error with `which-key' before that I am
    ;; working around with this.
    (add-to-list 'evil-buffer-regexps `(,(concat "\\`" (regexp-quote " *which-key*") "\\'")))))

;; BUG `which-key-preserve-window-configuration' breaks `+vertico-embark-which-key-indicator'.
(defadvice! cae-do-not-restore-wconf-in-minibuffer-a ()
  :before #'which-key--hide-buffer-side-window
  (when (minibufferp)
    (setq which-key--saved-window-configuration nil)))
;; BUG Allow C-h to open Consult when calling which-key without a prefix.
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

;; Do not scale fonts in `writeroom-mode'.
(setq +zen-text-scale 0)

(after! mule-util
  (setq truncate-string-ellipsis "...")) ;The unicode ellipsis is ugly to me

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

(when (modulep! :completion vertico)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target))
(after! eldoc
  (setq eldoc-echo-area-prefer-doc-buffer t)
  ;; BUG If Eldoc tries to show help while Which Key is active, there is an
  ;; error. Inhibit `eldoc' when `which-key' is active to prevent errors.
  (defadvice! cae-disable-eldoc-on-which-key-or-completion-a ()
    :before-until #'eldoc-documentation-default
    (and (featurep 'which-key) (which-key--popup-showing-p)))
  ;; BUG For some reason, Eldoc overrides the current buffer with a blank
  ;; *eldoc* buffer randomly while writing C++. I am testing if this fixes it.
  (setq eldoc-display-functions (delq 'eldoc-display-in-buffer eldoc-display-functions)))

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
      ;;("\\`\\*lsp-ui-imenu\\*\\'" :size cae-popup-shrink-to-fit :select t
      ;;:quit t
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
      ("\\`\\*detached-session-info\\*\\'" :quit t :select t :ttl 0)
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
      ("\\`\\*edit-indirect " :side top :select t :ttl 0 :size
       cae-popup-shrink-to-fit)
      ("\\`\\*vterm" :quit nil :ttl nil :size 0.3)
      ("\\`\\*eldoc\\*\\'" :quit t :size +popup-shrink-to-fit :ttl nil :side bottom)
      ("\\`\\*notmuch-hello" :ignore t)
      ("\\`\\*hackernews .*\\*'" :ignore t)
      ("\\`\\*gud-" :ttl nil :size 0.35)
      ("\\`\\*gptel-diff\\*" :ignore t)
      ("\\`\\*gptel-rewrite\\*" :ignore t)
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
  (after! magit                  ; Override Doom's popup rule for `transient'.
    ;; This is so that the `transient' popup takes up the entire horizontal
    ;; width of the window when I use `aider'.
    (set-popup-rule! "\\` \\*transient\\*\\'" :select nil :quit nil :ttl nil
      :size #'ignore))
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

(use-package! iscroll
  :defer t :init
  (add-hook 'org-mode-hook #'iscroll-mode)
  (add-hook 'markdown-mode-hook #'iscroll-mode)
  (add-hook 'image-mode-hook #'iscroll-mode)
  (add-hook 'eww-mode-hook #'iscroll-mode)
  (add-hook 'w3m-mode-hook #'iscroll-mode))

(use-package! beacon
  :defer 3.0 :config
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

(use-package! indent-bars
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
          "C-M-?"#'casual-main-menu)))
