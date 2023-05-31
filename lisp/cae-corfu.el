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
  (setopt orderless-component-separator #'orderless-escapable-split-on-space-or-ampersand))

(defun cae-corfu-enable-in-minibuffer-h ()
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (memq this-command '(evil-ex
                                   evil-ex-search-forward
                                   evil-ex-search-backward))
              (and (featurep 'helm-core)
                   (helm--alive-p))
              (corfu-mode +1))))
(add-hook 'minibuffer-setup-hook #'cae-corfu-enable-in-minibuffer-h)

(after! corfu
  (setopt corfu-preview-current (if (modulep! :private corfu +tng) 'insert nil)
          corfu-separator ?&
          corfu-auto-delay (if (modulep! :private corfu +tng) 0.0 0.2)
          corfu-on-exact-match nil
          corfu-preselect (if (modulep! :private corfu +tng) 'prompt t))
  ;; Fish completions are too slow for on-key completion.
  (setq-hook! 'eshell-mode-hook corfu-auto-delay 0.5)

  (when (modulep! :completion corfu +indexed)
    (setq corfu-indexed-start 1)
    (corfu-indexed-mode +1)
    (eval
     `(map! :map corfu-map
            "TAB" nil
            "<tab>" nil
            "RET" nil
            ,(cae-keyboard-kbd "C-" "1") (cae-generate-corfu-select-index 0)
            ,(cae-keyboard-kbd "C-" "2") (cae-generate-corfu-select-index 1)
            ,(cae-keyboard-kbd "C-" "3") (cae-generate-corfu-select-index 2)
            ,(cae-keyboard-kbd "C-" "4") (cae-generate-corfu-select-index 3)
            ,(cae-keyboard-kbd "C-" "5") (cae-generate-corfu-select-index 4)
            ,(cae-keyboard-kbd "C-" "6") (cae-generate-corfu-select-index 5)
            ,(cae-keyboard-kbd "C-" "7") (cae-generate-corfu-select-index 6)
            ,(cae-keyboard-kbd "C-" "8") (cae-generate-corfu-select-index 7)
            ,(cae-keyboard-kbd "C-" "9") (cae-generate-corfu-select-index 8)
            ,(cae-keyboard-kbd "C-" "0") (cae-generate-corfu-select-index 9)))))
