;;; lisp/cae-hydra.el -*- lexical-binding: t; -*-

(defvar cae-hydra--tab-bar-last-hydra-alist nil)
(defvar cae-hydra--last-hydra nil)

(defun cae-hydra-store-last-hydra-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (setq cae-hydra--last-hydra hydra-curr-body-fn)
    (hydra-keyboard-quit))
  (and (featurep 'persp-mode)
       (set-persp-parameter 'cae-hydra--last-hydra
                            cae-hydra--last-hydra))
  (setf (alist-get (tab-bar--current-tab)
                   cae-hydra--tab-bar-last-hydra-alist
                   nil nil #'eq)
        cae-hydra--last-hydra))

(defun cae-hydra-restore-last-hydra-h (&rest _)
  (let ((hydra (or (and (featurep 'persp-mode)
                        (persp-parameter 'cae-hydra--last-hydra))
                   (alist-get (tab-bar--current-tab)
                              cae-hydra--tab-bar-last-hydra-alist
                              nil nil #'eq)
                   cae-hydra--last-hydra)))
    (setq cae-hydra--last-hydra nil)
    (hydra-keyboard-quit)
    (cae-hydra-store-last-hydra-h)
    (funcall hydra)))

(add-hook 'persp-before-switch-functions #'cae-hydra-store-last-hydra-h)
(add-hook 'persp-activated-functions #'cae-hydra-restore-last-hydra-h)
(add-hook 'cae-tab-bar-before-switch-hook #'cae-hydra-store-last-hydra-h)
(add-hook 'cae-tab-bar-after-switch-hook #'cae-hydra-restore-last-hydra-h)
(add-hook 'minibuffer-setup-hook #'cae-hydra-store-last-hydra-h)
(add-hook 'minibuffer-exit-hook #'cae-hydra-restore-last-hydra-h)
