;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(add-hook #'doom-first-buffer-hook #'electric-pair-mode)

;;; UI
(load! "lisp/cae-theme")

(global-display-fill-column-indicator-mode +1)

(unless (modulep! :ui workspaces)
  ;; Unique buffer names
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; A minimal mouse-free `tab-bar' UI.
(defadvice! +tab-bar--load-buttons-a ()
  :after #'tab-bar--load-buttons
  (setq tab-bar-close-button   nil
        tab-bar-back-button    nil
        tab-bar-forward-button nil
        tab-bar-new-button     nil))

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

;; A minimal mouse-free `tab-bar' UI.
(defadvice! +tab-bar--load-buttons-a ()
  :after #'tab-bar--load-buttons
  (setq tab-bar-close-button   nil
        tab-bar-back-button    nil
        tab-bar-forward-button nil
        tab-bar-new-button     nil))

(setq x-stretch-cursor t
      truncate-string-ellipsis "…"
      scroll-margin 2
      kill-buffer-delete-auto-save-files t)

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

;;; Tools

;; Set up printers
(after! lpr (setq printer-name "Brother_HL-L2380DW_series"))
(after! ps-print (setq ps-printer-name "Brother_HL-L2380DW_series"))

(setq delete-by-moving-to-trash t
      history-length (expt 2 16))

(after! auth-source
  (setq auth-source-cache-expiry nil
        auth-sources (cl-remove-if (lambda (s) (string-suffix-p ".gpg" s))
                                   auth-sources)
        auth-source-gpg-encrypt-to nil))

;;; Editor

(delete-selection-mode +1)

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

;; Hide commands in M-x which do not work in the current mode. Vertico commands
;; are hidden in normal buffers.
(setq read-extended-command-predicate #'command-completion-default-include-p)

(use-package! pp+
  :after pp
  :init
  (map! [remap eval-expression] #'pp-eval-expression)
  :config
  (setq pp-read-expression-map minibuffer-local-map))

;;; Autocompletion

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
  (setq! copilot-node-executable (expand-file-name
                                  "~/.nvm/versions/node/v17.9.1/bin/node"))
  ;; Model our Copilot interface after Fish completions.
  (map! :map copilot-completion-map
        "<right>" #'copilot-accept-completion
        "C-f" #'copilot-accept-completion
        "M-<right>" #'copilot-accept-completion-by-word
        "M-f" #'copilot-accept-completion-by-word
        "C-e" #'copilot-accept-completion-by-line
        "<end>" #'copilot-accept-completion-by-line))
