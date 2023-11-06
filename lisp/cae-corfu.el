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
          ;; These are arguments to `replace-regexp-in-string'.
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

(when (modulep! :completion corfu +wildcard)
  (after! orderless
    ;; Orderless splits the string into components and then determines the
    ;; matching style for each component. This is all regexp stuff.
    (progn (setq orderless-component-separator
                 (cae-orderless-escapable-split-fn ?,))
           (after! corfu
             (setq corfu-separator ?,)
             (map! :map corfu-map
                   "," #'corfu-insert-separator)))))

(after! cape
  (setq cape-dabbrev-check-other-buffers t))
(after! corfu
  (setq corfu-preview-current (if (modulep! :completion corfu +tng) 'insert nil)
        corfu-auto-delay 0.05
        corfu-preselect (if (modulep! :completion corfu +tng) 'prompt t)
        tab-always-indent 'complete
        tab-first-completion 'eol)
  (after! corfu-quick
    (setq corfu-quick1 (cae-keyboard-kbd "asdfgh")
          corfu-quick2 (cae-keyboard-kbd "jkluionm")))

  ;; Fish completions for `emerge' are too slow for on-key completion.
  (when (executable-find "emerge")
    (setq-hook! 'fish-completion-mode-hook corfu-auto nil))

  ;; Glue between Copilot and Corfu.
  (defun cae-corfu-quit ()
    (interactive)
    (let ((copilot-state (and (bound-and-true-p copilot-mode)
                              (copilot--overlay-visible))))
      (corfu-quit)
      (when copilot-state
        (copilot-complete))))
  (add-hook! 'doom-escape-hook
    (defun cae-corfu-quit-h ()
      (when (cae-corfu-visible-p) (cae-corfu-quit) t)))
  (map! :map corfu-map
        [remap corfu-quit] #'cae-corfu-quit))

(after! lsp-completion
  ;; Do not try to configure `company-capf' for LSP.
  (setq lsp-completion-provider nil))
