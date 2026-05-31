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

;; Somehow this got into kill-emacs-hook without the package being loaded.
(defalias 'org-clock-kill-emacs-query #'ignore)

;; If `try' is used before the package list is loaded, fetch it.
(cae-defadvice! cae-hacks-try-package-refresh-contents-maybe (&rest _)
  :before #'try
  (unless package-archive-contents
    (package--archives-initialize)))

;; Address Claude Code getting killed. Should be addressed differently though.
(advice-add #'persp-kill-buffer-query-function :override
            #'cae-persp-kill-buffer-query-function)

(cae-defadvice! cae-hacks-debug-evil-local-tate (&rest _)
  :after #'evil-initialize-state
  (when (minibufferp)
    (backtrace)))
