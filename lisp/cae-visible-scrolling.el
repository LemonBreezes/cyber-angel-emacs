;;; lisp/cae-visible-scrolling.el -*- lexical-binding: t; -*-

(advice-add #'evil-scroll-up :before
            (cae-defun cae-evil-scroll-up-with-hint-a (count)
              (unless (= (line-end-position) (point-max))
                (require 'scrollkeeper)
                (let ((count (evil--get-scroll-count count)))
                  (save-excursion
                    (move-to-window-line (if (< count 0)
                                             -1
                                           0))
                    (funcall scrollkeeper-guideline-fn))))))

(advice-add #'evil-scroll-down :before
            (cae-defun cae-evil-scroll-up-with-hint-a (count)
              (unless (= (line-beginning-position) (point-min))
                (require 'scrollkeeper)
                (let ((count (evil--get-scroll-count count)))
                  (save-excursion
                    (move-to-window-line (if (< count 0)
                                             0
                                           -1))
                    (funcall scrollkeeper-guideline-fn))))))
