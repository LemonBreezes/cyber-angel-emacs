;;; ~/.doom.d/lisp/cae-corfu.el -*- lexical-binding: t; -*-

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
  (setq corfu-preview-current (if (modulep! :private corfu +tng) 'insert nil)
        corfu-auto-delay (if (modulep! :private corfu +tng) 0.0 0.2)
        corfu-on-exact-match nil
        corfu-preselect (if (modulep! :private corfu +tng) 'prompt t))
  (after! corfu-quick
    (setq corfu-quick1 (cae-keyboard-kbd "asdfgh")
          corfu-quick2 (cae-keyboard-kbd "jkl;")))
  ;; Fish completions are too slow for on-key completion.
  (setq-hook! 'eshell-mode-hook corfu-auto-delay 0.5)

  (defmacro cae-generate-corfu-select-index (index)
    "Return a named function to run `corfu-complete' for INDEX"
    `(cae-defun ,(intern (format "cae-corfu-complete-%s" index)) ()
       ,(format "Call `corfu-complete' for index %s." index)
       (interactive)
       (let ((corfu--index ,index))
         (corfu-complete))))

  (when (modulep! :private corfu +indexed)
    (setq corfu-indexed-start 1)
    (corfu-indexed-mode +1)
    (eval
     `(map! :map corfu-map
            "C-M-i" #'corfu-move-to-minibuffer
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
