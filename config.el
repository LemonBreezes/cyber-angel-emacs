;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;;; UI
(load! "lisp/cae-theme")

;; Do not use pagers
(setenv "PAGER" "cat")
(setenv "GIT_PAGER" "cat")

;; Set up fonts
(unless (memq system-type '(cygwin windows-nt ms-dos))
  (let ((font-size-offset (if (getenv "SSH_TTY") 0 2)))
    (setq doom-font (font-spec :family "Iosevka Comfy"
                               :size (+ 16 font-size-offset))
          doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo"
                                              :size (+ 16 font-size-offset))
          doom-unicode-font (unless (modulep! :ui unicode)
                              (font-spec :family "LXGW WenKai" :weight 'light
                                         :size (+ 15 font-size-offset))))))

(setq display-line-numbers-type 'relative)

;; Show minibuffer recursion depth
(autoload 'minibuffer-depth-setup "mb-depth")
(add-hook 'minibuffer-setup-hook  #'minibuffer-depth-setup)

(add-hook 'doom-first-buffer-hook #'global-page-break-lines-mode)

;; A minimal mouse-free `tab-bar' UI.
(defadvice! +tab-bar--load-buttons-a ()
  :after #'tab-bar--load-buttons
  (setq tab-bar-close-button   nil
        tab-bar-back-button    nil
        tab-bar-forward-button nil
        tab-bar-new-button     nil))

(setq x-stretch-cursor t
      truncate-string-ellipsis "..."
      scroll-margin 2
      kill-buffer-delete-auto-save-files t)

(after! which-key
  (setq which-key-ellipsis "..."
        which-key-compute-remaps t))

(after! eros
  (setq eros-eval-result-prefix "⟹ "))

;; Do not spam me with warnings
(after! warnings
  (setq warning-minimum-level :emergency))

(after! shr
  ;; I'm not a fan of variable-pitch fonts.
  (setq shr-use-fonts nil
        ;; Shr wraps lines in a visually unappealing way.
        shr-width 120
        shr-max-width 120)

  ;; Sometimes EWW makes web pages unreadable by adding a bright background. Do
  ;; not colorize backgrounds at all.
  (advice-add #'shr-colorize-region :around #'ignore))

(after! proced
  (setq-default proced-auto-update-flag t))

;; Allow switching to these buffers with `C-x b'
(add-hook 'compilation-mode-hook #'doom-mark-buffer-as-real-h)
(add-hook 'debugger-mode-hook #'doom-mark-buffer-as-real-h)

(when (modulep! :ui emoji)
  (after! emojify
    (dolist (mode '(comint-mode syslog-mode term-mode eshell-mode vterm-mode
                    shell-mode compilation-mode special-mode fundamental-mode
                    syslog-mode crontab-mode))
      (add-to-list 'emojify-inhibit-major-modes mode))))

(use-package! info-colors
  :defer t :init (add-hook 'Info-selection-hook 'info-colors-fontify-node))

(use-package! authinfo-color-mode
  :mode ("authinfo.gpg\\'" . authinfo-color-mode)
  :init (advice-add 'authinfo-mode :override #'authinfo-color-mode))

(use-package! topsy
  :defer t :init (add-hook 'prog-mode-hook #'topsy-mode)
  :config
  ;; It's really jarring that Topsy doesn't work if the top line is a comment.
  (setf (alist-get 'rjsx-mode topsy-mode-functions) #'cae-ui-topsy-rjsx-fn))

(when (modulep! :ui workspaces)
  ;; Add a tabline to Which Key
  (defadvice! rigor/which-key-show-workspace (orig-fun &rest pages-obj)
    "Show my workspaces in the echo thingy"
    :around #'which-key--process-page
    (let ((out (apply orig-fun pages-obj))
          (prefix-title (which-key--pages-prefix-title (car pages-obj))))
      (if (not (or (string-equal prefix-title "workspace")
                   (string-equal prefix-title "workspaces/windows")))
          out
        (cons (car out)
              `(lambda ()
                 (funcall ,(cdr out))
                 (unless (fboundp #'+workspace--tabline)
                   (load (symbol-file #'+workspace/new) nil t))
                 (which-key--echo (concat (current-message) " " (+workspace--tabline)))))))))

;; Set some popup rules.
(set-popup-rule! "^\\*Man [^*]*\\*" :size 0.5 :side 'right :quit t :select t)
(set-popup-rule! "^ \\*Metahelp\\*" :size 0.5 :side 'right :quit t :select t)
;; Do not kill or close these buffers (respectively windows)!
(set-popup-rule! "^\\*Backtrace\\*"      :size #'+popup-shrink-to-fit :ttl nil :quit nil)
(set-popup-rule! "^\\*exwm"              :size #'+popup-shrink-to-fit :ttl nil :quit nil)
(set-popup-rule! "^\\*Pp Eval Output\\*" :size #'+popup-shrink-to-fit :ttl nil :quit nil)
(set-popup-rule! "^\\*Help\\*"           :size #'+popup-shrink-to-fit :side 'right :quit t :select t :ttl nil)
(set-popup-rule! "^\\*Apropos\\*"        :size #'+popup-shrink-to-fit :quit t :select t :side 'right)
(after! info
  (set-popup-rule! "^\\*info\\*" :size #'+popup-shrink-to-fit :quit t :select t :side 'right :ttl nil))
(set-popup-rule! "^\\*helpful .*\\*"     :size #'+popup-shrink-to-fit :quit t :select t :ttl nil)
;; (set-popup-rule! "^\\*Messages\\*"       :size #'+popup-shrink-to-fit :quit t :select t :ttl nil)
;; (set-popup-rule! "^\\*Warnings\\*"       :size #'+popup-shrink-to-fit :quit t :select t :ttl nil)
(after! eww
    (set-popup-rule! "^\\*eww bookmarks\\*" :size 0.3 :side 'bottom :quit t :select t)
    (set-popup-rule! "^\\*eww" :ignore t))
(map! :map messages-buffer-mode-map :n "q" #'quit-window)

;; Lower the default popup delay.
(after! tooltip
  (setq tooltip-hide-delay 3))

(after! ace-link
  (add-to-list 'avy-styles-alist '(ace-link-man . pre)))



;;; Tools

(setq! browse-url-browser-function #'browse-url-generic
           browse-url-generic-program "chromium-bin"
           browse-url-generic-args '("--no-sandbox")
           browse-url-chromium-program "chromium-bin")

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'doom-large-file-excluded-modes 'nov-mode)

(add-to-list 'auto-mode-alist '("/var/log.*\\'" . syslog-mode))
(add-to-list 'auto-mode-alist '("\\.log$" . syslog-mode))
;; Do not highlight quoted strings in syslog-mode.
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

(make-directory (expand-file-name "secrets" doom-user-dir) t)
(dolist (file (directory-files
               (expand-file-name "secrets/" doom-user-dir) t "\\.el$"))
  (load file nil t))


(setq delete-by-moving-to-trash t
      history-length (expt 2 16))

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
    (projectile-discover-projects-in-search-path)))

(after! tramp
  (setq tramp-terminal-prompt-regexp "[[\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*\"]"))

;; Use Emacs as the default editor for shell commands.
(define-key (current-global-map) [remap async-shell-command] 'with-editor-async-shell-command)
(define-key (current-global-map) [remap shell-command] 'with-editor-shell-command)
(add-hook 'shell-mode-hook  #'with-editor-export-editor)
(add-hook 'eshell-mode-hook #'with-editor-export-editor)
(advice-add #'with-editor-export-editor :around #'doom-shut-up-a)

(when (and (modulep! :checkers spell)
           (not (modulep! :checkers spell +flyspell)))
  (after! spell-fu
    (add-to-list 'spell-fu-faces-exclude 'message-header-other)
    (add-to-list 'spell-fu-faces-exclude 'org-property-value)
    (add-to-list 'spell-fu-faces-exclude 'message-header-to)
    (setq spell-fu-faces-exclude
          (delq 'font-lock-string-face spell-fu-faces-include))))

(use-package! pdftotext
  :defer t
  :init
  (defadvice! +pdf-view-mode-a (oldfun &rest args)
    :around #'pdf-view-mode
    (if (display-graphic-p)
        (apply oldfun args)
      (apply #'pdftotext-mode args))))


;;; Editor

(load! "lisp/cae-multi")
(load! "lisp/cae-keyboard")
(load! "lisp/cae-repeat")
(map! "C-<f4>" #'tab-close)

(map! [remap backward-kill-word] #'doom/delete-backward-word
      [remap upcase-word] #'upcase-dwim
      [remap downcase-word] #'downcase-dwim
      [remap capitalize-word] #'capitalize-dwim
      [remap ispell-word] #'eac-ispell-word-then-abbrev
      (:after vertico
       :map vertico-map
       "<prior>" #'vertico-scroll-down
       "<next>" #'vertico-scroll-up)
      (:after man
       :map Man-mode-map
       :n "o" #'ace-link-man))

(map! :leader
      :desc "Copy link" "sy" #'link-hint-copy-link)

(when (and (not (modulep! editor evil))
           (modulep! :config default +bindings))
  (map! "C-@" #'er/expand-region
        "C-=" nil
        [remap doom/backward-to-bol-or-indent] #'beginning-of-line)
  (map! :leader "e" nil))

  (after! expand-region
    (setq! expand-region-autocopy-register "e"
           expand-region-smart-cursor t))

(advice-add #'persp-set-keymap-prefix :override #'ignore)

(setq set-mark-command-repeat-pop t
      next-line-add-newlines t)

(after! isearch
  (setq search-whitespace-regexp ".*?"
        isearch-lax-whitespace t
        isearch-wrap-pause 'no-ding
        isearch-lazy-count t
        isearch-repeat-on-direction-change t
        isearch-allow-motion t
        isearch-allow-scroll t
        isearch-yank-on-move 'shift
        isearch-motion-changes-direction t))

(after! ispell
  (setq ispell-quietly t
        ispell-dictionary "en_US"
        ispell-help-in-bufferp 'electric))

;; Make `smartparens' optional.
(unless (modulep! :config default +smartparens)
  (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
  (when (modulep! :term eshell)
    (after! eshell
      (remove-hook 'eshell-mode-hook #'smartparens-mode))))

;; Hide commands in M-x which do not work in the current mode. Vertico commands
;; are hidden in normal buffers.
(setq read-extended-command-predicate #'command-completion-default-include-p)

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


;;; Autocompletion
(when (modulep! :private corfu)
  (load! "lisp/cae-corfu"))

(after! dabbrev
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")
        dabbrev-upcase-means-case-search t))

(after! hippie-exp
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
  (add-hook 'doom-first-buffer-hook 'global-copilot-mode)
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
        "<end>" #'copilot-accept-completion-by-line)
  (when (modulep! :editor snippets)
    (add-hook 'yas-before-expand-snippet-hook #'copilot-clear-overlay)))

(after! isearch
  (use-package! isearch-dabbrev
    :defer t
    :init
    (map! :map isearch-mode-map
          "M-/" #'isearch-dabbrev-expand
          "C-M-/" #'isearch-dabbrev-expand)))

(when (modulep! :completion vertico)
  (map! "C-h C-m" #'consult-mode-command
        "C-h <return>" #'info-emacs-manual
        "C-x C-k C-k" #'consult-kmacro ; replaces
                                       ; `kmacro-end-or-call-macro-repeat',
                                       ; which is similar to
                                       ; `kmacro-end-and-call-macro' from `<f4>'
                                       ; and `C-x e'.
        ;; C-x bindings (ctl-x-map)
        "C-x M-:" #'consult-complex-command ;; orig. repeat-complex-command
        ;; Custom M-# bindings for fast register access
        ;;"M-'" #'consult-register-store ;; orig. abbrev-prefix-mark (unrelated)
        "C-x r SPC" #'consult-register-store ;; orig. abbrev-prefix-mark (unrelated)
        "C-x r J" #'consult-register
        [remap jump-to-register] #'consult-register-load
        ;; Other custom bindings
        ;; M-g bindings (goto-map)
        "M-g e" #'consult-compile-error
        (:when (modulep! :checkers syntax)
          "M-g f" #'consult-flycheck)
        (:when (modulep! :checkers syntax +flymake)
          "M-g f" #'consult-flymake) ;; Alternative: consult-flycheck
        "M-g g" #'consult-goto-line  ;; orig. goto-line
        "M-g M-g" #'consult-goto-line ;; orig. goto-line
        "M-g o" #'consult-outline     ;; Alternative: consult-org-heading
        "M-g m" #'consult-mark
        "M-g k" #'consult-global-mark
        "M-g i" #'consult-imenu
        "M-g I" #'consult-imenu-multi
        ;; M-s bindings (search-map)
        "M-s d" #'consult-find
        "M-s D" #'consult-locate
        "M-s g" #'consult-grep
        "M-s G" #'consult-git-grep
        "M-s r" #'consult-ripgrep
        "M-s l" #'consult-line
        "M-s L" #'consult-line-multi
        "M-s m" #'consult-multi-occur
        "M-s k" #'consult-keep-lines
        "M-s u" #'consult-focus-lines
        ;; Isearch integration
        "M-s e" #'consult-isearch-history
        :map isearch-mode-map
        "M-e" #'consult-isearch-history ;; orig. isearch-edit-string
        "M-s e" #'consult-isearch-history ;; orig. isearch-edit-string
        "M-s l" #'consult-line  ;; needed by consult-line to detect isearch
        "M-s L" #'consult-line-multi ;; needed by consult-line to detect isearch
        ;; Minibuffer history
        :map minibuffer-local-map
        "M-s" #'consult-history ;; orig. next-matching-history-element
        "M-r" #'consult-history)) ;; orig. previous-matching-history-element

(after! cc-mode
    (map! :map c-mode-base-map
          "TAB" #'indent-for-tab-command))

(when (modulep! :private corfu)
  (map! :leader
        (:prefix ("e" . "expand")
         :g "c" #'completion-at-point   ; capf
         :g "t" #'complete-tag          ; etags
         :g "d" #'cape-dabbrev          ; or dabbrev-completion
         :g "f" #'cape-file
         :g "k" #'cape-keyword
         :g "h" #'cape-history
         :g "s" #'cape-symbol
         :g "y" #'yasnippet-capf
         :g "a" #'cape-abbrev
         :g "i" #'cape-ispell
         :g "l" #'cape-line
         :g "w" #'cape-dict
         :g "\\" #'cape-tex
         :g "_" #'cape-tex
         :g "^" #'cape-tex
         :g "&" #'cape-sgml
         :g "r" #'cape-rfc1345
         :g ";" #'copilot-complete)))

;;; Term

(advice-add #'shell-completion-vars :after #'fish-completion-mode)

(after! em-glob
  (setq! eshell-error-if-no-glob nil))

(after! em-term
  ;; Some of the commands I copied from other configurations and will likely
  ;; never use.
  (setq! eshell-visual-commands
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
  (setq! calendar-week-start-day 1))

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
        ;; All my computers is 64-bit processors
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
  (define-prefix-command 'org-babel-map)
  (define-key org-mode-map org-babel-key-prefix #'org-babel-map))
