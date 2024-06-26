;;; lisp/cae-visible-scrolling.el -*- lexical-binding: t; -*-

(use-package! scrollkeeper
  :defer t :init
  (if (modulep! :editor evil)
      (map! [remap scroll-up-command] #'evil-scroll-page-down
            [remap scroll-down-command] #'evil-scroll-page-up)
    (map! [remap scroll-up-command] #'scrollkeeper-down
          [remap scroll-down-command] #'scrollkeeper-up))
  (setq scrollkeeper-scroll-steps 1
        scrollkeeper-scroll-step-delay 0)

  ;; These advices don't support scrolling in multiple steps but that is okay
  ;; with me. They also sometimes glitch out and don't let me scroll. But I
  ;; definitely need some kind of scroll guide since we don't have smooth scrolling.

  (defun cae-shared-scroll-with-hint-a (count line-fn count-fn)
    (require 'scrollkeeper)
    (let ((count (funcall count-fn count))
          (old-start (window-start)))
      (save-excursion
        (move-to-window-line (funcall line-fn count))
        (when (not (eq old-start (window-start)))
          (funcall scrollkeeper-guideline-fn)))))

  (defadvice! cae-evil-scroll-down-with-hint-a (count &rest _)
    :before #'evil-scroll-down
    (cae-shared-scroll-with-hint-a count
                                   (lambda (count) (if (< count 0) 0 -1))
                                   #'evil--get-scroll-count))

  (defadvice! cae-evil-scroll-up-with-hint-a (count &rest _)
    :before #'evil-scroll-up
    (cae-shared-scroll-with-hint-a count
                                   (lambda (count) (if (< count 0) -1 0))
                                   #'evil--get-scroll-count))

  (defadvice! evil-scroll-page-up-with-hint-a (count &rest _)
    :before #'evil-scroll-page-up
    (unless (> (abs count) 1)
      (cae-shared-scroll-with-hint-a count
                                     (lambda (count) (if (< count 0) -1 0))
                                     #'identity)))

  (defadvice! evil-scroll-page-down-with-hint-a (count &rest _)
    :before #'evil-scroll-page-down
    (unless (> (abs count) 1)
      (cae-shared-scroll-with-hint-a count
                                     (lambda (count) (if (< count 0) 0 -1))
                                     #'identity))))
