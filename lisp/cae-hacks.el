;;; lisp/cae-hacks.el -*- lexical-binding: t; -*-


;;; Other hacks

;; To migrate to Emacs31 without changing my config much.
(defalias 'hide-mode-line-mode 'mode-line-invisible-mode)

(defun cae-shut-up-a (fn &rest args)
  "Generic advisor for silencing noisy functions."
  (quiet!! (apply fn args)))

;; Make `advice-remove' ignore the keyword argument
(cae-defadvice! cae-hacks-advice-remove-ignore-keyword-args-a (args)
  :filter-args #'advice-remove
  (if (keywordp (nth 1 args))
      (list (nth 0 args) (nth 2 args))
    args))

;; Make `remove-hook' ignore the arguments after the first three.
(cae-defadvice! cae-hacks-remove-hook (args)
  :filter-args #'remove-hook
  (list (nth 0 args) (nth 1 args) (nth 2 args)))

;; Work around a garbage change in Emacs31 that made viewing diffs not work in
;; `save-some-buffers'.
(after! diff-mode
  (put 'diff-mode 'mode-class nil))

;; Somehow this got into kill-
(defalias 'org-clock-kill-emacs-query #'ignore)
