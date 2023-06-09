;;; ~/.doom.d/lisp/cae-corfu.el -*- lexical-binding: t; -*-

(if (modulep! corfu +ampersand)
    (progn (after! orderless
             ;; So Orderless splits the string into components and then determines the
             ;; matching style for each component. This is all regexp stuff.
             (setq orderless-component-separator #'cae-orderless-escapable-split-on-space-or-ampersand))
           (after! corfu
             (setq corfu-separator ?&)))
  (after! orderless
    (setq orderless-component-separator " +")))

(add-hook 'minibuffer-setup-hook #'cae-corfu-enable-in-minibuffer-h)
(add-hook 'minibuffer-exit-hook #'corfu-quit)

(after! cape
  (setq cape-dabbrev-check-other-buffers t))
(after! corfu
  (setq corfu-preview-current (if (modulep! :private corfu +tng) 'insert nil)
        corfu-auto-delay (if (modulep! :private corfu +tng) 0.0 0.2)
        corfu-on-exact-match nil
        corfu-preselect (if (modulep! :private corfu +tng) 'prompt t)
        ;;tab-always-indent 'complete
        tab-first-completion 'word-or-paren
        )
  (after! corfu-quick
    (setq corfu-quick1 (cae-keyboard-kbd "asdfgh")
          corfu-quick2 (cae-keyboard-kbd "jkl;")))
  ;; Fish completions are too slow for on-key completion.
  (setq-hook! 'eshell-mode-hook corfu-auto-delay 0.5)
  (when (modulep! :private corfu +numbers)
    (corfu-indexed-mode +1)))

(when (modulep! :editor snippets)
  (use-package! cape-yasnippet
    :defer t :init
    (map! :map cae-completion-mode-map
          "C-. s" #'cape-yasnippet)
    (dolist (hook '(prog-mode-hook
                    text-mode-hook
                    lsp-mode-hook
                    sly-mode-hook))
      (add-hook hook #'cae-yas-setup-capf))
    :config
    (add-to-list 'completion-at-point-functions 'cape-yasnippet)))

(when (modulep! :lang org)
  (use-package! org-block-capf
    :defer t :init
    (add-hook 'org-mode-hook #'org-block-capf-add-to-completion-at-point-functions)))
