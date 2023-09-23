;;; lisp/cae-ansi-color.el -*- lexical-binding: t; -*-

(add-hook! text-mode
  (unless (derived-mode-p 'org-mode)
    ;; Apply ANSI color codes
    (with-silent-modifications
      (ansi-color-apply-on-region (point-min) (point-max) t))))
                                        ;
