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
        tab-always-indent 'complete
        tab-first-completion 'eol)
  (after! corfu-quick
    (setq corfu-quick1 (cae-keyboard-kbd "asdfgh")
          corfu-quick2 (cae-keyboard-kbd "jkl;")))
  ;; Fish completions are too slow for on-key completion.
  (setq-hook! 'fish-completion-mode-hook corfu-auto-delay nil)

  (defun cae-corfu-quit ()
    (interactive)
    (let ((copilot-state (and (bound-and-true-p copilot-mode)
                              (copilot--overlay-visible))))
      (corfu-quit)
      (when copilot-state
        (copilot-complete))))
  (add-hook 'doom-escape-hook
            (cae-defun cae-corfu-quit-h ()
              (when (cae-corfu-visible-p)
                (cae-corfu-quit)
                t)))
  (map! :map corfu-map
        "C-g" #'cae-corfu-quit)
  (add-hook 'evil-normal-state-exit-hook #'corfu-quit))

(after! lsp-completion
  ;; Do not try to configure `company-capf' for LSP.
  (setq lsp-completion-provider nil))

(when (modulep! :lang org)
  (use-package! org-block-capf
    :defer t :init
    (add-hook 'org-mode-hook #'org-block-capf-add-to-completion-at-point-functions)))
