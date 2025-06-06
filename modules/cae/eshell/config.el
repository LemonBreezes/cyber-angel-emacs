;;; cae/eshell/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

;;; EAT Terminal Integration

(use-package! eat
  :defer t :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  :config
  ;; Do not let EAT override TERM.
  (setq eat-term-name (lambda () eshell-term-name)
        eat-enable-yank-to-terminal t)

  ;; Temporarily disable some modes when EAT is active.
  (defun cae-eshell--disable-modes-in-eat ()
    "Disable certain modes when EAT is running and restore them when it stops."
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
                (set mode-var nil))))))))
  
  (add-hook 'eat--eshell-process-running-mode-hook #'cae-eshell--disable-modes-in-eat)

  ;; It's kind of hard to figure out how to exit char mode, so let's give a hint.
  (cae-defadvice! cae-eat-eshell-print-char-mode-hint-a ()
    :after #'eat-eshell-char-mode
    (message "Type M-RET/C-M-m to exit char mode.")))

;;; Command Handling

;; Doom overrides `eshell/emacs' with a custom function. I prefer for `emacs'
;; to work in Eshell as it does in a terminal.
(when (symbol-function #'eshell/emacs)
  (setf (symbol-function #'eshell/e)
        (symbol-function #'eshell/emacs))
  (unintern 'eshell/emacs obarray))

;; Expand abbreviations before parsing input.
(cae-defadvice! cae-eshell-expand-abbrev-a (&rest _)
  :before #'eshell-send-input
  (expand-abbrev))

;;; Bookmarks and Buffer Handling

(use-package! eshell-bookmark
  :defer t :init
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

;; Colorize ansi escape sequences in exported buffers
(cae-advice-add #'eshell-output-object-to-target :around #'cae-eshell-ansi-buffer-output)

;; Parse buffer redirection >#buf and >#.
(after! esh-arg
  (add-hook 'eshell-parse-argument-hook #'cae-eshell-syntax-buffer-redirect))

;;; History Management

;; Filter trivial commands from history.
(defun cae-eshell-input-filter (str)
  "Filter some trivial commands from the input history."
  (not (or (string-blank-p str)
           (equal "cd" str)
           (string-prefix-p "cd " str)
           (string-prefix-p " " str)
           (string-match-p "^[a-zA-Z]\\'" str))))

(setq eshell-input-filter #'cae-eshell-input-filter)

(after! em-hist
  (setq eshell-history-size nil
        eshell-history-append t
        eshell-hist-ignoredups 'erase)
  (add-to-list 'eshell-expand-input-functions
               #'eshell-expand-history-references))

;;; Prompt Configuration

;; Set the prompt
(autoload 'epe-theme-lambda "eshell-prompt-extras")
(after! eshell
  (setq eshell-highlight-prompt nil
        eshell-prompt-function #'epe-theme-lambda)
  (after! eshell-prompt-extras
    (setq epe-show-local-working-directory t
          epe-show-git-status-extended t)))

;;; Input Handling

;; Don't leave me with unbalanced delimiters.
(cae-defadvice! cae-eshell-kill-input-with-delimiters-a ()
  :after #'eshell-kill-input
  (let ((beg (pos-bol))
        (end (pos-eol)))
    (while (and (not (eq (point) end))
                (condition-case _
                    (scan-sexps beg end)
                  (scan-error t)))
      (delete-char 1)))
  (unless (eq (char-syntax (char-before)) ?\s)
    (insert-char ?\s)))

;;; Module Configuration

(after! esh-module
  (add-to-list 'eshell-modules-list 'eshell-elecslash))

(autoload 'eshell-elecslash-initialize "em-elecslash")
(add-hook 'eshell-mode-hook #'eshell-elecslash-initialize)

(after! em-glob
  (setq eshell-glob-splice-results t))

;;; Keybindings

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

;;; Atuin Integration

(use-package! eshell-atuin
  :when (executable-find "atuin")
  :after eshell :config
  (eshell-atuin-mode))

;;Local Variables:
;;eval: (unless (modulep! :cae eshell) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
