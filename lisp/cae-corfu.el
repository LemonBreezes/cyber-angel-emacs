;;; ~/.doom.d/lisp/cae-corfu.el -*- lexical-binding: t; -*-

(after! orderless
  ;; So Orderless splits the string into components and then determines the
  ;; matching style for each component. This is all regexp stuff.
  (defun orderless-escapable-split-on-space-or-ampersand (s)
    (mapcar
     (lambda (piece)
       (thread-last piece
                    (replace-regexp-in-string
                     (concat (string 0) "\\|" (string 1))
                     (lambda (x)
                       (pcase x
                         ("\0" " ")
                         ("\1" "&")
                         (_ x))))
                    (replace-regexp-in-string (string 1) "&")))
     (split-string (replace-regexp-in-string
                    "\\\\\\\\\\|\\\\ \\|\\\\&"
                    (lambda (x)
                      (pcase x
                        ("\\ " "\0")
                        ("\\&" "\1")
                        (_ x)))
                    s 'fixedcase 'literal)
                   "[ &]+" t)))
  (setq orderless-component-separator #'orderless-escapable-split-on-space-or-ampersand))

(defun cae-corfu-enable-in-minibuffer-h ()
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (string-match-p "I-search: " (minibuffer-prompt))
              (string-match-p "Query replace " (minibuffer-prompt))
              (string-match-p "Align regexp" (minibuffer-prompt))
              (string-match-p "Filter:" (minibuffer-prompt))
              (memq this-command '(evil-ex
                                   evil-ex-search-forward
                                   evil-ex-search-backward))
              (and (featurep 'helm-core)
                   (helm--alive-p))
              (corfu-mode +1))))
(add-hook 'minibuffer-setup-hook #'cae-corfu-enable-in-minibuffer-h)

(after! cape
  (setq cape-dabbrev-check-other-buffers t))
(after! corfu
  (setq corfu-preview-current (if (modulep! :private corfu +tng) 'insert nil)
        corfu-auto-delay (if (modulep! :private corfu +tng) 0.0 0.2)
        corfu-on-exact-match nil
        corfu-separator ?&
        corfu-preselect (if (modulep! :private corfu +tng) 'prompt t)
        tab-always-indent 'complete)
  (after! corfu-quick
    (setq corfu-quick1 (cae-keyboard-kbd "asdfgh")
          corfu-quick2 (cae-keyboard-kbd "jkl;")))
  ;; Fish completions are too slow for on-key completion.
  (setq-hook! 'eshell-mode-hook corfu-auto-delay 0.5)
  (when (modulep! :private corfu +indexed)
    (corfu-indexed-mode +1)
    (eval
     `(map! :map corfu-map
            "C-M-i" #'corfu-move-to-minibuffer)))
  (when (modulep! :editor multiple-cursors)
    (after! multiple-cursors-core 
      (add-to-list 'mc/unsupported-minor-modes 'corfu-mode))))
