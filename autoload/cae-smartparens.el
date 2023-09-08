;;; autoload/cae-smartparens.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-evil-cleverparens-hydra/body "autoload/cae-smartparens" nil t)
(defhydra cae-evil-cleverparens-hydra (:color pink :foreign-keys run)
  ("<f6>" nil "Exit" :exit t)
  ("M-(" evil-cp-wrap-next-round :column "Wrap")
  ("M-)" evil-cp-wrap-previous-round :column "Wrap")
  ("M-C" evil-cp-change-enclosing "Change enclosing" :column "Insert")
  ("M-D" evil-cp-delete-enclosing "Delete enclosing" :column "Delete")
  ("M-J" sp-join-sexp "Join sexp" :column "Delete")
  ("M-O" evil-cp-open-above-form "Copy above form" :column "Insert")
  ("M-R" evil-cp-raise-form "Raise form" :column "Delete")
  ("M-S" sp-split-sexp "Split sexp" :column "Insert")
  ("M-T" evil-cp-toggle-balanced-yank "Toggle balanced yank" :column "Misc")
  ("M-Y" evil-cp-yank-enclosing "Copy enclosing form" :column "Misc")
  ("M-[" evil-cp-wrap-next-square :column "Wrap")
  ("M-]" evil-cp-wrap-previous-square :column "Wrap")
  ("M-a" evil-cp-insert-at-end-of-form "Insert at end of form" :column "Movement")
  ("M-c" evil-cp-change-sexp "Change sexp" :column "Delete")
  ("M-d" evil-cp-delete-sexp "Delete sexp" :column "Delete")
  ("M-h" evil-cp-beginning-of-defun "Beginning of defun" :column "Movement")
  ("M-i" evil-cp-insert-at-beginning-of-form "Insert at beginning of form" :column "Movement")
  ("M-j" evil-cp-drag-forward "Drag forward" :column "Change")
  ("M-k" evil-cp-drag-backward "Drag backward" :column "Change")
  ("M-l" evil-cp-end-of-defun "End of defun" :column "Movement")
  ("M-o" evil-cp-open-below-form "Open below form" :column "Movement")
  ("M-q" sp-indent-defun "Indent defun" :column "Change")
  ("M-r" sp-raise-sexp "Raise sexp" :column "Delete")
  ("M-s" sp-splice-sexp "Splice sexp" :column "Delete")
  ("M-t" sp-transpose-sexp "Transpose sexp" :column "Change")
  ("M-v" sp-convolute-sexp "Convolute sexp" :column "Change")
  ("M-w" evil-cp-copy-paste-form "Copy paste form" :column "Insert")
  ("M-y" evil-cp-yank-sexp "Copy sexp from point" :column "Misc")
  ("M-z" evil-cp-override "Evil CP override" :column "Misc")
  ("M-{" evil-cp-wrap-next-curly :column "Wrap")
  ("M-}" evil-cp-wrap-previous-curly :column "Wrap")
  ("M-\"" +evil-cp-wrap-next-double-quotes :column "Wrap")
  )

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
