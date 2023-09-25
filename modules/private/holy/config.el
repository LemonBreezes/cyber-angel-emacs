;;; private/holy/config.el -*- lexical-binding: t; -*-

(load! "+visible-mark")
(load! "+multiple-cursors")
(load! "+restore-point")

;; Allow escape to exit the minibuffer.
(define-key! :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit)

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
      [remap er/expand-region] #'eri/expand-region
      (:when (modulep! :tools lookup)
       [remap xref-find-definitions] #'cae-lookup-definition-dwim))

;; This minor mode is defined so that there keybindings can be temporarily
;; turned off for multiple cursors and similar modes where completion is not a
;; good idea.
(define-prefix-command 'cae-completion-prefix-map)
(define-key cae-completion-prefix-map "c" #'completion-at-point)
(define-key cae-completion-prefix-map "t" #'complete-tag)
(define-key cae-completion-prefix-map "d" #'cape-dabbrev)
(define-key cae-completion-prefix-map "f" #'cape-file)
(define-key cae-completion-prefix-map "k" #'cape-keyword)
(define-key cae-completion-prefix-map "h" #'cape-history)
(define-key cae-completion-prefix-map "e" #'cape-symbol)
(define-key cae-completion-prefix-map "a" #'cape-abbrev)
(define-key cae-completion-prefix-map "l" #'cape-line)
(define-key cae-completion-prefix-map "w" #'cape-dict)
(define-key cae-completion-prefix-map "\\" #'cape-tex)
(define-key cae-completion-prefix-map "_" #'cape-tex)
(define-key cae-completion-prefix-map "^" #'cape-tex)
(define-key cae-completion-prefix-map "&" #'cape-sgml)
(define-key cae-completion-prefix-map "r" #'cape-rfc1345)
(define-key cae-completion-prefix-map "." #'copilot-complete)
(when (modulep! :editor multiple-cursors)
  (define-key cae-completion-prefix-map (kbd "C-.") #'mc/unfreeze-fake-cursors)
  (define-key cae-completion-prefix-map (kbd "<f5>") #'mc/unfreeze-fake-cursors))
(define-minor-mode cae-completion-mode
  "A minor mode for convenient completion keybindings."
  :global t
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-.") #'cae-completion-prefix-map)
            (define-key map (kbd "<f5>") #'cae-completion-prefix-map)
            map)
  :group 'cae)
(cae-completion-mode +1)

(use-package! goggles
  :defer t :init
  (add-hook 'prog-mode-hook #'goggles-mode)
  (add-hook 'text-mode-hook #'goggles-mode)
  (add-hook 'conf-mode-hook #'goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package! zop-to-char
  :defer t :init
  (map! [remap zap-to-char] #'zop-to-char
        [remap zap-up-to-char] #'zop-up-to-char)
  :config
  (setq zop-to-char-kill-keys '(?\C-m ?\C-k ?\C-w)))

;; Type `?' during `rectangle-mark-mode' for a help buffer describing the
;; `speedrect' commands.
(use-package! speedrect
  :after-call rectangle-mark-mode-hook
  :config
  (speedrect-hook))

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

(use-package! string-edit-at-point    ; Used in `cae-edit-indirect-dwim'.
  :defer t)

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


(use-package! isearch-mb
  :after-call isearch-mode-hook
  :config
  (isearch-mb--setup)
  (isearch-mb-mode +1)
  (dolist (cmd '(recenter-top-bottom reposition-window
                 scroll-right scroll-left isearch-yank-word
                 consult-isearch-history))
    (add-to-list 'isearch-mb--with-buffer cmd))
  (dolist (cmd '(anzu-isearch-query-replace anzu-isearch-query-replace-regexp
                 avy-isearch consult-line))
    (add-to-list 'isearch-mb--after-exit cmd))
  (define-key isearch-mb-minibuffer-map (kbd "C-w") #'isearch-yank-word)
  (define-key isearch-mb-minibuffer-map (kbd "M-j") #'avy-isearch)
  (when (modulep! :completion vertico)
    (map! :map isearch-mb-minibuffer-map
          [remap consult-history] #'consult-isearch-history)
    (define-key isearch-mb-minibuffer-map (kbd "M-s l") 'consult-line))
  (define-key isearch-mb-minibuffer-map (kbd "M-%")   #'anzu-isearch-query-replace)
  (define-key isearch-mb-minibuffer-map (kbd "M-s %") #'anzu-isearch-query-replace-regexp))

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
;;   (when (modulep! :completion corfu)
;;     (after! corfu
;;       (map! :map corfu-map
;;             "M-j" #'corfu-quick-jump
;;             "M-i" #'corfu-quick-insert))))

;;Local Variables:
;;eval: (unless (modulep! :private holy) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
