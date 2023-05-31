;;; private/misc-applications/autoload/trashed.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +trashed-revert-buffer-a (oldfun)
  (when (prog1 (buffer-live-p trashed-buffer)
          (funcall oldfun))
    (revert-buffer)))

;;;###autoload
(defun +trashed-hide-cursor-h ()
  (setq-local cursor-type nil
              evil-normal-state-cursor '(bar . 0)
              evil-insert-state-cursor '(bar . 0)
              evil-visual-state-cursor '(box . 0)
              evil-motion-state-cursor '(box . 0)
              evil-replace-state-cursor '(hbar . 0)
              evil-operator-state-cursor '(hbar . 0)
              evil-emacs-state-cursor '(hbar . 0)))
