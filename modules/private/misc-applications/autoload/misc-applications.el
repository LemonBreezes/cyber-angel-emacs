;;; private/misc-applications/autoload/misc-applications.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +misc-applications-hide-cursor-h ()
  (setq-local cursor-type nil)
  (when (boundp 'hl-line-mode)
    (hl-line-mode +1))
  (when (featurep 'evil)
    (setq-local evil-normal-state-cursor '(bar . 0)
                evil-insert-state-cursor '(bar . 0)
                evil-visual-state-cursor '(box . 0)
                evil-motion-state-cursor '(box . 0)
                evil-replace-state-cursor '(hbar . 0)
                evil-operator-state-cursor '(hbar . 0)
                evil-emacs-state-cursor '(hbar . 0))))
