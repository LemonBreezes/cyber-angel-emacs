;;; autoload/cae-smartparens.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+evil-cp-wrap-next-double-quotes "autoload/cae-evil" nil t)
(evil-define-command +evil-cp-wrap-next-double-quotes (count)
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
