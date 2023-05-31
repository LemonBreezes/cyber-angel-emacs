;;; private/eshell/config.el -*- lexical-binding: t; -*-

(after! eshell
  (setq-hook! 'eshell-mode-hook
    imenu-generic-expression
    `((,(propertize "λ" 'face 'eshell-prompt) "^.* λ \\(.*\\)" 1)))

  (eat-eshell-mode +1)
  (eat-eshell-visual-command-mode +1)

  ;; Do not let EAT override TERM.
  (setq! eat-term-name (lambda () eshell-term-name))

  (map! :map (eat-eshell-semi-char-mode-map)
        "C-a" #'eat-self-input
        "C-e" #'eat-self-input
        "M-DEL" #'eat-self-input
        "C-u" #'eat-self-input
        "M->" #'end-of-buffer
        "<prior>" #'scroll-down-command
        "<next>" #'scroll-up-command)
  (map! :map eat-mode-map
        "C-c C-u" (cmd! (eat-input-char ?\C-u 1)))
  

  (add-hook 'eshell-mode-hook #'+eshell-set-up-autocompletion)

  ;; Expand abbreviations before parsing input.
  (advice-add 'eshell-send-input :before #'expand-abbrev)

  (when (modulep! :config default +smartparens)
    (sp-local-pair 'eshell-mode "#<" ">")
    (sp-local-pair 'eshell-mode "$(" ")")
    (sp-local-pair 'eshell-mode "${" "}"))
  (unless (modulep! :config default +smartparens)
    (remove-hook 'eshell-mode-hook #'smartparens-mode))

  (use-package! eshell-buffer-capf
    :defer t
    :init
    (add-hook 'eshell-mode-hook #'eshell-buffer-capf-setup))

  (when (modulep! :private corfu)
    (add-hook 'eshell-mode-hook
              (lambda ()
                (add-to-list 'completion-at-point-functions 'cape-abbrev))))

  (use-package eshell-bookmark
    :after eshell
    :config
    (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

  ;; Colorize ansi escape sequences in exported buffers
  (advice-add #'eshell-output-object-to-target :around #'+eshell-ansi-buffer-output)

  ;; Parse buffer redirection >#buf and >#.
  (add-hook 'eshell-parse-argument-hook #'+eshell-syntax-buffer-redirect)

  (setq eshell-input-filter #'+eshell-input-filter)

  (after! esh-module
    (when (>= emacs-major-version 29)
      (add-to-list 'eshell-modules-list 'eshell-elecslash))
    (add-to-list 'eshell-modules-list 'eshell-rebind)
    (after! em-rebind
      (setq eshell-rebind-keys-alist nil
            eshell-cannot-leave-input-list
            (cl-set-difference eshell-cannot-leave-input-list
                               '(previous-line next-line)))))

  (when (>= emacs-major-version 29)
    (autoload 'eshell-elecslash-initialize "eshell-elecslash")
    (add-hook 'eshell-mode-hook #'eshell-elecslash-initialize))


  (after! em-hist
    (setq! eshell-history-size (expt 2 16))
    (add-to-list 'eshell-expand-input-functions #'eshell-expand-history-references))

  (cond ((modulep! :completion vertico)
         (map! :map eshell-mode-map
               :ig "M-R" #'consult-history))
        ((modulep! :completion ivy)
         (map! :map eshell-mode-map
               :ig "M-R" #'counsel-esh-history))
        ((modulep! :completion helm)
         (map! :map eshell-mode-map
               :ig "M-R" #'helm-eshell-history)))

  (map! :map eshell-mode-map
        :g "C-l" #'+eshell-clear
        :g [remap doom/forward-to-last-non-comment-or-eol] #'end-of-line
        :g [remap +kill-region-or-backward-to-bol] #'eshell-kill-input
        :i "RET" #'eshell-send-input)

  (after! esh-mode
    (map! :map eshell-mode-map))

  ;; (add-to-list 'eshell-modules-list 'eshell-smart)
  ;; (after! em-smart
  ;;   (setq! eshell-where-to-jump 'begin
  ;;          eshell-review-quick-commands t
  ;;          eshell-smart-space-goes-to-end t))

  ;; ;; Glue `em-smart' with Evil.
  ;; ;; Not sure why using `eshell-smart-display-navigate-list' doesn't work for this.
  ;; (defadvice! +eshell-smart-disply-move-a (orig-fn &rest args)
  ;;   :around #'eshell-smart-display-move
  ;;   (if (memq this-command '(evil-normal-state
  ;;                            evil-escape))
  ;;       (let ((this-command 'self-insert-command))
  ;;         (apply orig-fn args))
  ;;     (apply orig-fn args)))

  ;; (use-package! esh-autosuggest
  ;;   :defer t
  ;;   :init
  ;;   (add-hook 'eshell-mode-hook #'esh-autosuggest-mode))

  ;; From this PR https://github.com/doomemacs/doomemacs/pull/6867/files
  (load! "+fish-completion-annotation-fix")

  (load! "ha-eshell"))
