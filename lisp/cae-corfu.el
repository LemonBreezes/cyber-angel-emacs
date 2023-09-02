;;; ~/.doom.d/lisp/cae-corfu.el -*- lexical-binding: t; -*-

(defmacro cae-orderless-escapable-split-fn (char)
  `(defun cae-orderless-escapable-split-on-space-or-char (s)
     (mapcar
      (lambda (piece)
        (replace-regexp-in-string
         (string 1) ,(string char)
         (replace-regexp-in-string
          (concat (string 0) "\\|" (string 1))
          (lambda (x)
            (pcase x
              ("\0" " ")
              ("\1" ,(string char))
              (_ x)))
          piece
          'fixedcase 'literal)
         'fixedcase 'literal))
      (split-string (replace-regexp-in-string
                     (concat "\\\\\\\\\\|\\\\ \\|\\\\" ,(string char))
                     (lambda (x)
                       (pcase x
                         ("\\ " "\0")
                         (,(concat "\\" (string char)) "\1")
                         (_ x)))
                     s 'fixedcase 'literal)
                    ,(concat "[ " (string char) "]+") t))))

(after! orderless
  (if (modulep! :private corfu +split-char)
      ;; So Orderless splits the string into components and then determines the
      ;; matching style for each component. This is all regexp stuff.
      (progn (setq orderless-component-separator
                   (cae-orderless-escapable-split-fn ?,))
             (after! corfu
               (setq corfu-separator ?,)
               (map! :map corfu-map
                     "," #'corfu-insert-separator)))
    (setq orderless-component-separator " +")))

(add-hook 'minibuffer-setup-hook #'cae-corfu-enable-in-minibuffer-h)
(add-hook 'minibuffer-exit-hook #'corfu-quit)

(after! cape
  (setq cape-dabbrev-check-other-buffers t
        cape-line-buffer-function #'cae-cape-line-buffers))
(after! corfu
  (setq corfu-preview-current (if (modulep! :private corfu +tng) 'insert nil)
        corfu-auto-delay 0.05
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
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit))

(after! lsp-completion
  ;; Do not try to configure `company-capf' for LSP.
  (setq lsp-completion-provider nil))

;; Text mode

(add-hook! (org-mode markdown-mode)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(when (modulep! :lang org)
  (use-package! org-block-capf
    :defer t :init
    (add-hook 'org-mode-hook #'org-block-capf-add-to-completion-at-point-functions)))
