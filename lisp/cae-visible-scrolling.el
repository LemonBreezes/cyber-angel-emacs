;;; lisp/cae-visible-scrolling.el -*- lexical-binding: t; -*-

(use-package! scrollkeeper
  :defer t :init
  (map! [remap scroll-up-command] #'scrollkeeper-contents-up
        [remap scroll-down-command] #'scrollkeeper-contents-down)
  (setq scrollkeeper-scroll-steps 1)

  ;; These advices don't support scrolling in multiple steps but that is fine
  ;; for me.
  (advice-add #'evil-scroll-down :before
              (cae-defun cae-evil-scroll-down-with-hint-a (count)
                (unless (= (line-end-position) (point-max))
                  (require 'scrollkeeper)
                  (let ((count (evil--get-scroll-count count)))
                    (save-excursion
                      (move-to-window-line (if (< count 0) 0 -1))
                      (funcall scrollkeeper-guideline-fn))))))

  (advice-add #'evil-scroll-up :before
              (cae-defun cae-evil-scroll-up-with-hint-a (count)
                (unless (= (line-beginning-position) (point-min))
                  (require 'scrollkeeper)
                  (let ((count (evil--get-scroll-count count)))
                    (save-excursion
                      (move-to-window-line (if (< count 0) -1 0))
                      (funcall scrollkeeper-guideline-fn))))))

  (advice-add #'evil-scroll-page-up :before
              (cae-defun evil-scroll-page-up-with-hint-a (count)
                (unless (> (abs count) 1)
                  (require 'scrollkeeper)
                  (save-excursion
                    (move-to-window-line (if (< count 0) -1 0))
                    (funcall scrollkeeper-guideline-fn)))))

  (advice-add #'evil-scroll-page-down :before
              (cae-defun evil-scroll-page-down-with-hint-a (count)
                (unless (> (abs count) 1)
                  (require 'scrollkeeper)
                  (save-excursion
                    (move-to-window-line (if (< count 0) 0 -1))
                    (funcall scrollkeeper-guideline-fn))))))
