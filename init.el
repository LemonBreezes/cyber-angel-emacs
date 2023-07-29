;;; init.el -*- lexical-binding: t; -*-

;; Don't enable these options. They will break your config.
(defvar cae-config-compilation-on-kill-enabled-p nil
  "Whether on-kill native compilation is enabled.")
(defvar cae-config-incremental-compilation-enabled-p nil
  "Whether incremental native compilation is enabled.")

;;Since I sometimes compile my config, I want to make sure that I'm always
;;loading the latest version.
(setq load-prefer-newer (not (or cae-config-compilation-on-kill-enabled-p
                                 cae-config-incremental-compilation-enabled-p)))
(setq doom-theme 'wheatgrass)           ;Set a dark fallback theme.

(when (version< "30.0" emacs-version)
  (add-to-list 'safe-local-variable-directories doom-user-dir)
  (add-to-list 'safe-local-variable-directories doom-emacs-dir))

(load! "lisp/cae-debug")                ;Debug Emacs.
(load! "lisp/cae-lib")
(load! "lisp/cae-hacks")

(setq native-comp-async-jobs-number (num-processors))

;; Do not override other keymaps with `general-override-mode'. This was created
;; because Doom's leader key was overriding Eat's `eat-self-input' keybinding.
(advice-add #'general-override-mode :override #'ignore)
(after! general
  (define-minor-mode cae-general-override-mode
    "Minor mode to enable `general-override-mode-map' without
overriding other keymaps."
    :global t
    :init-value nil
    :lighter nil
    :keymap general-override-mode-map)
  (add-hook 'cae-general-override-mode-hook
    (cae-defun cae-general--unbind-keys ()
      ;; Do not override `org-edit-special' in Org mode.
      (define-key general-override-mode-map (kbd "C-c '") nil)))
  (add-hook 'doom-after-init-hook #'cae-general-override-mode t))

;; Also show keybinidng descriptions on my second leader key when using Evil.
(when (or (featurep 'evil) (autoloadp 'evil-mode))
  (defun cae-which-key-add-key-based-replacements-a
      (oldfun key-sequence &rest args)
    (when (string-prefix-p "SPC " key-sequence)
      (apply oldfun (replace-regexp-in-string "^SPC " "DEL " key-sequence) args))
    (apply oldfun key-sequence args))

  (advice-add #'which-key-add-key-based-replacements :around
              #'cae-which-key-add-key-based-replacements-a))

;; I added these to help with debugging my config. It's easier to toggle these
;; than to comment out large sections of my config.
(defvar cae-init-ui-enabled-p t
  "Whether our UI section of `config.el' is enabled.")
(defvar cae-init-tools-enabled-p t
  "Whether our tools section of `config.el' is enabled.")
(defvar cae-init-editor-enabled-p t
  "Whether our editor section of `config.el' is enabled.")
(defvar cae-init-autocompletion-enabled-p t
  "Whether our autocompletion section of `config.el' is enabled.")
(defvar cae-keyboard-remaps-enabled-p t
  "Whether we remap keys for special keyboard layouts.")

(doom! :completion
       (vertico +icons +childframe)

       :ui
       doom-dashboard
       hl-todo
       hydra
       (ligatures +extra +iosevka)
       ;;nav-flash
       (popup +defaults)
       (vc-gutter +pretty +diff-hl)
       vi-tilde-fringe
       workspaces
       ;;(window-select +switch-window)

       :editor
       file-templates
       format
       snippets
       multiple-cursors
       lispy
       fold
       word-wrap
       ;; (evil +everywhere) ; Be prepared to encounter issues if you use Evil
       ;; with my config. If you encounter any or have suggestions, please raise
       ;; an issue on GitHub.

       :emacs
       undo
       ibuffer
       vc

       :term
       eshell
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) vterm)

       :checkers
       (syntax +childframe)
       ;;spell
       ;;grammar

       :tools
       ;; debugger
       direnv
       editorconfig
       (eval +overlay)
       (lookup +dictionary +docsets +offline)
       (magit) ;; +forge is broken for me on the latest Emacs.
       (lsp +peek)
       tree-sitter

       :lang
       (emacs-lisp +tree-sitter)
       (org +roam2 +tree-sitter)
       (cc +lsp +tree-sitter)
       (web +lsp +tree-sitter)
       (sh +lsp +tree-sitter)
       (nix +lsp +tree-sitter)
       (solidity +lsp +tree-sitter)
       (lua +lsp +tree-sitter +fennel)

       :os
       (:if IS-MAC macos)
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

       :config
       (default +bindings +smartparens)

       :email
       (mu4e +org)
       ;;(notmuch +org)

       :app
       (rss +org)

       :private
       (corfu +numbers)
       helm
       (debugger +lsp)
       (dirvish +icons +dirvish)
       eshell
       exwm
       dired
       lisp
       misc-applications
       modeline
       unpackaged
       vc
       org)

;; Local Variables:
;; eval: (when (featurep 'lispy) (lispy-mode -1))
;; End:
