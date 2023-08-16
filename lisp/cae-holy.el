;;; lisp/cae-holy.el -*- lexical-binding: t; -*-

(unless (modulep! :editor evil)
  (setq doom-leader-alt-key "C-c"
        doom-localleader-alt-key "C-c l"
        doom-leader-key "C-c"
        doom-localleader-key "C-c l"))


(use-package! goggles
  :when (not (modulep! :editor evil))
  :defer t :init
  (add-hook 'prog-mode-hook #'goggles-mode)
  (add-hook 'text-mode-hook #'goggles-mode)
  (add-hook 'conf-mode-hook #'goggles-mode)
  :config
  (setq-default goggles-pulse t))


(use-package! symbol-overlay
  :when (not (modulep! :editor evil))
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
  (unless (modulep! :editor evil)
    (map! :leader "'" #'cae-edit-indirect-dwim))
  :config
  (add-hook 'edit-indirect-after-creation-hook
            (cae-defun cae-edit-indirect-major-mode-fallback-h ()
              (when (eq major-mode 'fundamental-mode)
                (funcall (buffer-local-value
                          'major-mode
                          (overlay-buffer edit-indirect--overlay)))))))
