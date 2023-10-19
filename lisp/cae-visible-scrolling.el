;;; lisp/cae-visible-scrolling.el -*- lexical-binding: t; -*-

(use-package! scrollkeeper
  :defer t :init
  (map! [remap scroll-up-command] #'scrollkeeper-contents-up
        [remap scroll-down-command] #'scrollkeeper-contents-down)
  (setq scrollkeeper-scroll-steps 1)

  ;; These advices don't support scrolling in multiple steps but that is okay
  ;; with me.
  (defun cae-evil-scroll-with-hint-a (point-fn edge-fn line-fn)
    (unless (funcall edge-fn (funcall point-fn) (funcall line-fn))
      (require 'scrollkeeper)
      (let ((count (evil--get-scroll-count count)))
        (save-excursion
          (move-to-window-line (if (< count 0) 0 -1))
          (funcall scrollkeeper-guideline-fn)))))

  (defadvice! cae-evil-scroll-down-with-hint-a (count &rest _)
    :before #'evil-scroll-down
    (cae-evil-scroll-with-hint-a #'line-end-position #'= #'point-max))

  (defadvice! cae-evil-scroll-up-with-hint-a (count &rest _)
    :before #'evil-scroll-up
    (cae-evil-scroll-with-hint-a #'line-beginning-position #'= #'point-min))

  (defadvice! evil-scroll-page-up-with-hint-a (count &rest _)
    :before #'evil-scroll-page-up
    (cae-evil-scroll-with-hint-a #'abs #'>= 1))

  (defadvice! evil-scroll-page-down-with-hint-a (count &rest _)
    :before #'evil-scroll-page-down
    (cae-evil-scroll-with-hint-a #'abs #'>= 1)))
