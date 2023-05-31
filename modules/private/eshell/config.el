;;; private/eshell/config.el -*- lexical-binding: t; -*-

;; Detached's database is getting corrupted, so I'm disabling it for now.
;;(use-package! detached
;;  :defer t :init
;;  (add-hook 'doom-first-input-hook #'detached-init)
;;  (map! [remap async-shell-command] #'detached-shell-command
;;        [remap compile] #'detached-compile
;;        [remap recompile] #'detached-recompile
;;        (:when (modulep! :completion vertico)
;;         [remap detached-open-session] #'detached-consult-session)
;;        :leader
;;        :prefix "o"
;;        :desc "Detached session" "s" #'detached-open-session)
;;  :custom ((detached-show-output-on-attach t)
;;           (detached-terminal-data-command system-type)))

;; Output to stdout instead of a pager like `less'. This is useful when
;; redirecting output to buffers like for `eshell' or `shell'.
(setenv "PAGER" "cat")
(setenv "GIT_PAGER" "cat")

(use-package! eat
  :defer t :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(after! eshell
  (setq-hook! 'eshell-mode-hook
    imenu-generic-expression
    `((,(propertize "λ" 'face 'eshell-prompt) "^.* λ \\(.*\\)" 1)))

  (unless cae-config-finished-loading
    (eat-eshell-mode +1)
    (eat-eshell-visual-command-mode +1))

  ;; Doom overrides `eshell/emacs' with a custom function. I prefer for `emacs'
  ;; to work in Eshell as it does in a terminal.
  (when (symbol-function #'eshell/emacs)
    (setf (symbol-function #'eshell/e)
          (symbol-function #'eshell/emacs))
    (unintern 'eshell/emacs))

  ;; Do not let EAT override TERM.
  (setq eat-term-name (lambda () eshell-term-name)
        eat-enable-yank-to-terminal t)
  ;;(map! :map (eat-eshell-semi-char-mode-map)
  ;;      "C-a" #'eat-self-input
  ;;      "C-e" #'eat-self-input
  ;;      "M-DEL" #'eat-self-input
  ;;      "C-u" #'eat-self-input
  ;;      "M->" #'end-of-buffer
  ;;      "<prior>" #'scroll-down-command
  ;;      "<next>" #'scroll-up-command)
  ;;(map! :map eat-mode-map
  ;;      "C-c C-u" (cmd! (eat-input-char ?\C-u 1)))
  (advice-add #'eat-eshell-char-mode
              :after
              (cae-defun cae-eat-eshell-print-char-mode-hint-a ()
                (message "Type M-RET/C-M-m to exit char mode.")))
  (add-hook 'eshell-mode-hook #'cae-eshell-set-up-autocompletion)

  ;; Expand abbreviations before parsing input.
  (advice-add 'eshell-send-input :before
              (cae-defun cae-eshell-expand-abbrev-a (&rest _)
                (expand-abbrev)))

  (use-package eshell-bookmark
    :after eshell
    :config
    (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

  ;; Colorize ansi escape sequences in exported buffers
  (advice-add #'eshell-output-object-to-target :around #'cae-eshell-ansi-buffer-output)

  ;; Parse buffer redirection >#buf and >#.
  (add-hook 'eshell-parse-argument-hook #'cae-eshell-syntax-buffer-redirect)

  ;; Filter trivial commands from history.
  (setq eshell-input-filter #'cae-eshell-input-filter)

  (after! esh-opt
    ;; Python virtualenvs
    ;; (require 'virtualenvwrapper)
    ;; (venv-initialize-eshell)
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function #'epe-theme-lambda)
    (after! eshell-prompt-extras
      (setq epe-show-local-working-directory t)))
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
    (setq eshell-history-size (expt 2 16))
    (add-to-list 'eshell-expand-input-functions #'eshell-expand-history-references))

  (cond ((modulep! :completion vertico)
         (map! :map eshell-mode-map
               [remap eshell-isearch-backward] #'consult-history))
        ((modulep! :completion ivy)
         (map! :map eshell-mode-map
               [remap eshell-isearch-backward] #'counsel-esh-history))
        ((modulep! :completion helm)
         (map! :map eshell-mode-map
               [remap eshell-isearch-backward] #'helm-eshell-history)))

  (map! :map eshell-mode-map
        "C-l" #'cae-eshell-clear
        (:when (modulep! :private corfu)
         "TAB" #'completion-at-point
         "<tab>" #'completion-at-point))

  ;; From this PR https://github.com/doomemacs/doomemacs/pull/6867/files
  (load! "+fish-completion-annotation-fix")

  (load! "ha-eshell"))
