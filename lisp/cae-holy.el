;;; lisp/cae-holy.el -*- lexical-binding: t; -*-

(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l"
      doom-leader-key "C-c"
      doom-localleader-key "C-c l")

;; Doom should not bind leader key prefixes to keys which are not alphanumeric
;; because then they can be overwriting other packages' keybindings. As an
;; example, Org mode has `C-c !' bound to `org-time-stamp-inactive' and `C-c &'
;; bound to `org-mark-ring-goto'.
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (setq flycheck-keymap-prefix (kbd "C-c C"))
  (map! :leader (:prefix-map ("C" . "checkers"))))

;; I like to add bind `<leader> h' to `help-map' like how Doom Emacs does for
;; Evil.
(map! :leader :desc "help" "h" help-map)

;; Doom binds it's folding prefix to `C-c C-f' which is a keybinding used by
;; many major modes.
(when (modulep! :editor fold)
  (defvar doom-fold-map (lookup-key doom-leader-map "\C-f"))
  (define-key doom-leader-map "\C-f" nil)
  (map! :leader
        (:prefix-map ("F" . "fold")
         "k" #'vimish-fold-delete
         "K" #'vimish-fold-delete-all
         "c" #'vimish-fold
         "t" #'+fold/toggle
         "C" #'+fold/close-all
         "o" #'+fold/open
         "O" #'+fold/open-all)))

(map! [remap doom/backward-to-bol-or-indent] #'beginning-of-line
      (:when (modulep! :tools lookup)
       [remap xref-find-definitions] #'cae-lookup-definition-dwim))


(use-package! goggles
  :defer t :init
  (add-hook 'prog-mode-hook #'goggles-mode)
  (add-hook 'text-mode-hook #'goggles-mode)
  (add-hook 'conf-mode-hook #'goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package! symbol-overlay
  :defer t :init
  (map! "M-i" #'symbol-overlay-put
        "M-I" #'symbol-overlay-remove-all
        "M-N" #'symbol-overlay-switch-forward ;jump to the next overlay
        "M-P" #'symbol-overlay-switch-backward)
  (map! :leader
        :desc "Highlight symbol at point" "to" #'symbol-overlay-mode)
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  :config
  (map! :map symbol-overlay-map
        "<f6>" #'cae-symbol-overlay-cheatsheet
        "N" #'symbol-overlay-switch-forward
        "P" #'symbol-overlay-switch-backward
        "r" #'symbol-overlay-rename
        "-" #'negative-argument
        "o" #'cae-avy-symbol-at-point)
  ;; LSP and Eglot provide its own symbol highlighting.
  (add-hook! (lsp-mode eglot-managed-mode) (symbol-overlay-mode -1))
  ;; For some reason `symbol-overlay-switch-backward' jumps to the first symbol
  ;; overlay in the buffer. This is probably a bug.
  (advice-add #'symbol-overlay-get-list
              :around #'cae-hacks-symbol-overlay-reverse-list-a)
  (defun cae-hacks-symbol-overlay-reverse-list-a (oldfun &rest args)
    (if (eq (car args) -1)
        (nreverse (apply oldfun args))
      (apply oldfun args))))

(use-package! edit-indirect
  :defer t :init
  (map! :leader "'" #'cae-edit-indirect-dwim)
  :config
  (add-hook 'edit-indirect-after-creation-hook
            (cae-defun cae-edit-indirect-major-mode-fallback-h ()
              (when (eq major-mode 'fundamental-mode)
                (funcall (buffer-local-value
                          'major-mode
                          (overlay-buffer edit-indirect--overlay)))))))

;; (unless (modulep! :editor evil)
;;   (map! :prefix "C-z"
;;         "n" #'avy-goto-line-below
;;         "p" #'avy-goto-line-above
;;         "t" #'tabgo
;;         ;;"y" #'avy-copy-region
;;         "c" #'avy-goto-char
;;         ;;"m" #'avy-move-region
;;         "l" #'avy-goto-line
;;         "e" #'avy-goto-end-of-line
;;         "." #'cae-avy-symbol-at-point
;;         ;;"k" #'avy-kill-region
;;         ;;"w" #'avy-kill-ring-save-region
;;         "a" #'cae-avy-embark-act-on-region
;;         "j" #'avy-goto-word-1
;;         (:when (modulep! :editor fold)
;;          "f" #'vimish-fold-avy)
;;         "o" #'switch-window
;;         "0" #'switch-window-then-delete
;;         "1" #'switch-window-then-maximize
;;         "2" #'switch-window-then-split-horizontally
;;         "3" #'switch-window-then-split-vertically
;;         "4" #'switch-window-then-kill-buffer
;;         ;;"r" #'avy-resume ; `avy-resume' is too buggy to be useful.
;;         "SPC" #'avy-goto-char-timer
;;         (:map isearch-mode-map
;;          "j" #'avy-isearch))
;;   (when (modulep! :completion vertico)
;;     (after! vertico
;;       (map! :map vertico-map
;;             "M-j" #'vertico-quick-jump
;;             "M-i" #'vertico-quick-exit)))
;;   (after! embark
;;     (map! :map embark-collect-mode-map
;;           "M-j" #'avy-embark-collect-choose
;;           "M-i" #'avy-embark-collect-act))
;;   (when (modulep! :private corfu)
;;     (after! corfu
;;       (map! :map corfu-map
;;             "M-j" #'corfu-quick-jump
;;             "M-i" #'corfu-quick-insert))))


