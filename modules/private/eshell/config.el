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

;;Set the prompt
(autoload 'epe-theme-lambda "eshell-prompt-extras")
(after! em-prompt
  (setq eshell-highlight-prompt nil
        eshell-prompt-function #'epe-theme-lambda)
  (after! eshell-prompt-extras
    (setq epe-show-local-working-directory t)))

(after! esh-module
  (add-to-list 'eshell-modules-list 'eshell-elecslash))

(autoload 'eshell-elecslash-initialize "eshell-elecslash")
(add-hook 'eshell-mode-hook #'eshell-elecslash-initialize)

(after! em-hist
  (setq eshell-history-size (expt 2 16))
  (add-to-list 'eshell-expand-input-functions
               #'eshell-expand-history-references))

(after! eshell
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
        :ig "C-d" #'cae-eshell-quit-or-delete-char))

;; From this PR https://github.com/doomemacs/doomemacs/pull/6867/files.
(load! "eshell-doc-doom-pr-6867")

(load! "ha-eshell")
(load! "ambrevar-global-eshell-history")
