;;; private/eshell/config.el -*- lexical-binding: t; -*-

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
;;  :custom ((detached-terminal-data-command system-type)))

;; This package just became broken on Emacs HEAD.
(use-package! eat
  :defer t :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  :config
  ;; Do not let EAT override TERM.
  (setq eat-term-name (lambda () eshell-term-name)
        eat-enable-yank-to-terminal t)

  ;; Temporarily disable some modes when EAT is active.
  (add-hook 'eat--eshell-process-running-mode-hook
            (defun cae-eshell-disable-modes-in-eat-h ()
              (let ((modes '(corfu-mode eldoc-mode)))
                (dolist (mode modes)
                  (when (boundp mode)
                    (let ((mode-var (intern (concat "cae-eshell--" (symbol-name mode) "-enabled-p"))))
                      (make-local-variable mode-var)
                      (if eat--eshell-process-running-mode
                          (progn (set mode-var (symbol-value mode))
                                 (funcall mode -1))
                        (when (symbol-value mode-var)
                          (funcall mode 1)
                          (set mode-var nil)))))))))

  ;; It's kind of hard to figure out how to exit char mode, so let's give a hint.
  (advice-add #'eat-eshell-char-mode
              :after
              (cae-defun cae-eat-eshell-print-char-mode-hint-a ()
                (message "Type M-RET/C-M-m to exit char mode."))))

(setq-hook! 'eshell-mode-hook
  imenu-generic-expression
  `((,(propertize "λ" 'face 'eshell-prompt) "^.* λ \\(.*\\)" 1)))

;;Doom overrides `eshell/emacs' with a custom function. I prefer for `emacs'
;;to work in Eshell as it does in a terminal.
(when (symbol-function #'eshell/emacs)
  (setf (symbol-function #'eshell/e)
        (symbol-function #'eshell/emacs))
  (unintern 'eshell/emacs))

(add-hook 'eshell-mode-hook
          (cae-defun cae-eshell-set-up-autocompletion ()
            (add-hook 'completion-at-point-functions
                      #'cape-file nil t)))

;; Expand abbreviations before parsing input.
(advice-add 'eshell-send-input :before
            (cae-defun cae-eshell-expand-abbrev-a (&rest _)
              (expand-abbrev)))

(use-package eshell-bookmark
  :defer t :init
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

;; Colorize ansi escape sequences in exported buffers
(advice-add #'eshell-output-object-to-target :around #'cae-eshell-ansi-buffer-output)

;; Parse buffer redirection >#buf and >#.
(after! esh-arg
  (add-hook 'eshell-parse-argument-hook #'cae-eshell-syntax-buffer-redirect))

;; Filter trivial commands from history.
(setq eshell-input-filter
      (cae-defun cae-eshell-input-filter (str)
        "Filter some trivial commands from the input history."
        (not (or (string-blank-p str)
                 (equal "cd" str)
                 (string-prefix-p "cd " str)
                 (string-prefix-p " " str)
                 (string-match-p "^[a-zA-Z]$" str)))))

;; Set the prompt
(autoload 'epe-theme-lambda "eshell-prompt-extras")
(after! eshell
  (setq eshell-highlight-prompt nil
        eshell-prompt-function #'epe-theme-lambda)
  (after! eshell-prompt-extras
    (setq epe-show-local-working-directory t)))

(after! esh-module
  (add-to-list 'eshell-modules-list 'eshell-elecslash))

(autoload 'eshell-elecslash-initialize "em-elecslash")
(add-hook 'eshell-mode-hook #'eshell-elecslash-initialize)

(after! em-hist
  (setq eshell-history-size nil
        eshell-hist-ignoredups 'erase)
  (add-to-list 'eshell-expand-input-functions
               #'eshell-expand-history-references))

(after! eshell
  (when (modulep! :completion vertico)
    (map! :map eshell-mode-map
          [remap +eshell/search-history] #'consult-history)
    (after! em-hist
      (map! :map eshell-hist-mode-map
            "M-s" nil
            "M-r" #'consult-history)))
  (map! :map eshell-mode-map
        "C-l" #'cae-eshell-clear
        "C-S-l" #'cae-sudo-toggle
        :i "C-u" #'eshell-kill-input
        :ig "C-d" #'cae-eshell-quit-or-delete-char
        [remap doom/backward-to-bol-or-indent] #'beginning-of-line))

;; From this PR https://github.com/doomemacs/doomemacs/pull/6867/files.
(load! "eshell-doc-doom-pr-6867")

;;Local Variables:
;;eval: (unless (modulep! :private eshell) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
