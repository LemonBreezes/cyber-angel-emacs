;;; private/misc-applications/autoload/misc-applications.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +misc-applications-hide-cursor-h ()
  (setq-local cursor-type nil)
  (hl-line-mode +1)
  (setq-local evil-normal-state-cursor '(bar . 0)
              evil-insert-state-cursor '(bar . 0)
              evil-visual-state-cursor '(box . 0)
              evil-motion-state-cursor '(box . 0)
              evil-replace-state-cursor '(hbar . 0)
              evil-operator-state-cursor '(hbar . 0)
              evil-emacs-state-cursor '(hbar . 0)))

(defmacro +misc-applications-with-cursor-hidden (&rest body)
  `(let ((cursor-type-old cursor-type)
         (evil-normal-state-cursor-old evil-normal-state-cursor)
         (evil-insert-state-cursor-old evil-insert-state-cursor)
         (evil-visual-state-cursor-old evil-visual-state-cursor)
         (evil-motion-state-cursor-old evil-motion-state-cursor)
         (evil-replace-state-cursor-old evil-replace-state-cursor)
         (evil-operator-state-cursor-old evil-operator-state-cursor)
         (evil-emacs-state-cursor-old evil-emacs-state-cursor))
     (setq-local cursor-type nil
                 evil-normal-state-cursor '(bar . 0)
                 evil-insert-state-cursor '(bar . 0)
                 evil-visual-state-cursor '(box . 0)
                 evil-motion-state-cursor '(box . 0)
                 evil-replace-state-cursor '(hbar . 0)
                 evil-operator-state-cursor '(hbar . 0)
                 evil-emacs-state-cursor '(hbar . 0))
     (unwind-protect (progn ,@body)
       (setq-local cursor-type cursor-type-old
                   evil-normal-state-cursor evil-normal-state-cursor-old
                   evil-insert-state-cursor evil-insert-state-cursor-old
                   evil-visual-state-cursor evil-visual-state-cursor-old
                   evil-motion-state-cursor evil-motion-state-cursor-old
                   evil-replace-state-cursor evil-replace-state-cursor-old
                   evil-operator-state-cursor evil-operator-state-cursor-old
                   evil-emacs-state-cursor evil-emacs-state-cursor-old))))
