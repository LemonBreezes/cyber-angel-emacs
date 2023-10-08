;;; autoload/cae-smartparens.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-evil-cp-wrap-next-double-quotes "autoload/cae-evil" nil t)
(evil-define-command cae-evil-cp-wrap-next-double-quotes (count)
  "Wraps the next COUNT sexps inside curly braces. If the point
is inside a symbol, that symbol is treated as the first sexp to
wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the next N forms, where N is the count for how
many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-universal-invoke-arg-count)))
        (save-excursion
          (sp-backward-up-sexp)
          (evil-cp--wrap-next "\"" count))
        (evil-cp--backward-up-list))
    (evil-cp--wrap-next "\"" count)))

;;;###autoload
(defun cae-sp-raise-sexp ()
  "Like `sp-raise-sexp', but works on the region if active."
  (interactive)
  (if (region-active-p)
      (save-excursion
        (let ((beg (region-beginning))
              (end (region-end)))
          (goto-char end)
          (delete-region end (progn (sp-up-sexp) (point)))
          (goto-char beg)
          (delete-region beg (progn (sp-backward-up-sexp) (point)))))
    (call-interactively #'sp-raise-sexp)))

;;;###autoload
(defun cae-sp-which-key-cheatsheet () (interactive))
(hercules-def :toggle-funs #'cae-sp-which-key-cheatsheet
              :keymap 'smartparens-mode-map
              :transient t
              :flatten t)
