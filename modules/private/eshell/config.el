;;; private/eshell/config.el -*- lexical-binding: t; -*-

;; Currently broken.
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
;;  (defun patch--detached--db-update-sessions (orig-fn)
;;    "Ensure we print the full object to the DB."
;;    (let ((print-length nil)
;;          (print-level nil))
;;      (funcall orig-fn)))
;;
;;  (advice-add 'detached--db-update-sessions
;;              :around
;;              #'patch--detached--db-update-sessions)
;;  :custom ((detached-terminal-data-command system-type)))

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

(after! eshell
  (setq-hook! 'eshell-mode-hook
    imenu-generic-expression
    `((,(propertize "λ" 'face 'eshell-prompt) "^.* λ \\(.*\\)" 1)))

  ;; Doom overrides `eshell/emacs' with a custom function. I prefer for `emacs'
  ;; to work in Eshell as it does in a terminal.
  (when (symbol-function #'eshell/emacs)
    (setf (symbol-function #'eshell/e)
          (symbol-function #'eshell/emacs))
    (unintern 'eshell/emacs))

  (add-hook 'eshell-mode-hook #'cae-eshell-set-up-autocompletion)

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
      (add-to-list 'eshell-modules-list 'eshell-elecslash)))

  (when (>= emacs-major-version 29)
    (autoload 'eshell-elecslash-initialize "eshell-elecslash")
    (add-hook 'eshell-mode-hook #'eshell-elecslash-initialize))

  (after! em-hist
    (setq eshell-history-size (expt 2 16))
    (add-to-list 'eshell-expand-input-functions
                 #'eshell-expand-history-references))

  (cond ((modulep! :completion vertico)
         (map! :map eshell-mode-map
               [remap eshell-isearch-backward] #'consult-history
               "M-R" #'consult-history))
        ((modulep! :completion ivy)
         (map! :map eshell-mode-map
               [remap eshell-isearch-backward] #'counsel-esh-history))
        ((modulep! :completion helm)
         (map! :map eshell-mode-map
               [remap eshell-isearch-backward] #'helm-eshell-history)))

  (map! :map eshell-mode-map
        "C-l" #'cae-eshell-clear
        "C-S-l" #'cae-sudo-toggle
        :ig "C-d" #'cae-eshell-quit-or-delete-char
        (:when (modulep! :private corfu)
         "TAB" #'completion-at-point
         "<tab>" #'completion-at-point))

  ;; From this PR https://github.com/doomemacs/doomemacs/pull/6867/files.
  (load! "+fish-completion-annotation-fix")

  (load! "ha-eshell")

  ;; Global Eshell history. From https://gitlab.com/ambrevar/dotfiles/-/blob/master/.emacs.d/lisp/init-eshell.el.
  (defvar ambrevar/eshell-history-global-ring nil
    "The history ring shared across Eshell sessions.")

  (defun ambrevar/eshell-hist-use-global-history ()
    "Make Eshell history shared across different sessions."
    (unless ambrevar/eshell-history-global-ring
      (when eshell-history-file-name
        (eshell-read-history nil t))
      (setq ambrevar/eshell-history-global-ring (or eshell-history-ring (make-ring eshell-history-size))))
    (setq eshell-history-ring ambrevar/eshell-history-global-ring))
  (add-hook 'eshell-mode-hook 'ambrevar/eshell-hist-use-global-history)

  (defun ambrevar/ring-delete-first-item-duplicates (ring)
    "Remove duplicates of last command in history.
Return RING.

This should be faster then `seq-uniq'.  Unlike
`eshell-hist-ignoredups' or `comint-input-ignoredups', it does
not allow duplicates ever.
Surrounding spaces are ignored when comparing."
    (let ((first (ring-ref ring 0))
          (index 1))
      (while (<= index (1- (ring-length ring)))
        (if (string= (string-trim first)
                     (string-trim (ring-ref ring index)))
            ;; REVIEW: We could stop at the first match, it would be faster and it
            ;; would eliminate duplicates if we started from a fresh history.
            ;; From an existing history that would not clean up existing
            ;; duplicates beyond the first one.
            (ring-remove ring index)
          (setq index (1+ index))))
      ring))

  (defun ambrevar/eshell-history-remove-duplicates ()
    (ambrevar/ring-delete-first-item-duplicates eshell-history-ring))
  (add-hook 'eshell-pre-command-hook 'ambrevar/eshell-history-remove-duplicates)

  ;; Always save history
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history))
