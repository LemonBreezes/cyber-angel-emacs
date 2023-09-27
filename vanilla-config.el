;;; vanilla-config.el -*- lexical-binding: t; -*-

(setopt create-lockfiles nil
        inhibit-startup-screen t
        suggest-key-bindings nil)

(with-eval-after-load 'org
  (setopt org-agenda-files ',(progn (require 'org)
                                    org-agenda-files)))

(set-language-environment "UTF-8")

;; Set up some editing conveniences
(electric-pair-mode 1)
(electric-indent-mode 1)
(show-paren-mode 1)
(global-eldoc-mode 1)
(abbrev-mode 1)
(icomplete-mode 1)
(icomplete-vertical-mode 1)
(setopt tab-always-indent 'complete)

;; Set up modeline and appearance
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(load-theme 'wheatgrass)

;; This is incomplete but at least it sets some variables up
(setq delete-by-moving-to-trash t
      remote-file-name-inhibit-delete-by-moving-to-trash t
      remote-file-name-inhibit-auto-save t
      remote-file-name-inhibit-auto-save-visited t
      make-cursor-line-fully-visible nil ;I forgot why I set this.
      yank-pop-change-selection t
      browse-url-firefox-new-window-is-tab t
      comint-history-isearch 'dwim
      compilation-environment '("LANG=C" "TERM=dumb")
      completion-auto-select 'second-tab
      completions-detailed t
      completions-format 'vertical
      completions-group t
      completions-group-sort 'alphabetical
      custom-buffer-done-kill t
      dired-isearch-filenames 'dwim
      global-mark-ring-max 1024
      grep-program "rg"
      help-enable-symbol-autoload t
      help-enable-completion-autoload t
      help-enable-symbol-autoload t
      help-window-select t
      help-clean-buttons t
      help-enable-variable-value-editing t
      Info-fontify-maximum-menu-size t
      grep-use-headings t
      history-delete-duplicates t
      history-length t
      mark-ring-max 1024
      message-log-max t
      save-place-limit nil
      save-place-save-skipped nil
      kill-ring-max 1024
      kill-whole-line t
      list-matching-lines-jump-to-current-line t
      mouse-prefer-closest-glyph t
      next-error-message-highlight 'keep
      read-char-by-name-sort 'code
      revert-buffer-quick-short-answers t
      scroll-error-top-bottom t
      scroll-preserve-screen-position t
      shift-select-mode 'permanent
      smiley-style t
      tar-mode-show-date t
      track-eol t
      tramp-allow-unsafe-temporary-files t
      visual-order-cursor-movement t
      view-read-only t
      what-cursor-show-names t
      xref-search-program 'ripgrep)

;; For preventing errors evaluating my Doom config
(defvar doom-user-dir "~/.config/doom")
(defvar cae-multi-local-dir (expand-file-name "shared-local/" doom-user-dir))
(defvar cae-multi-data-dir (expand-file-name "etc/" cae-multi-local-dir))
(defvar cae-multi-cache-dir (expand-file-name "cache/" cae-multi-local-dir))
(defvar cae-multi-secrets-dir (expand-file-name "secrets/" doom-user-dir))

;; Load some Doom library functions so that I can evaluate code from my private
;; config without Doom running.
(let ((doom-lib "~/.config/emacs/lisp/doom-lib"))
  (when (file-exists-p doom-lib)
    (load doom-lib t t)))

;; Bootstrap straight
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                    'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;Local Variables:
;;eval: (remove-hook 'write-file-functions #'eval-buffer t)
;;End:
