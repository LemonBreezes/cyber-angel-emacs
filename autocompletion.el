;;; autocompletion.el -*- lexical-binding: t; -*-

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
  (setq helm-split-window-default-side 'right))
