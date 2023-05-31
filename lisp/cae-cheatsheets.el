;;; lisp/cae-cheatsheets.el -*- lexical-binding: t; -*-

(defvar cae-cheatsheets-minibuffer--last-hydra nil)
(defvar cae-cheatsheets-minibuffer--last-workspace nil)
(defvar cae-cheatsheets-minibuffer--last-tab nil)
(defvar cae-cheatsheets-minibuffer--last-tab-index nil)

(defun cae-cheatsheets-minibuffer-hydra-pause-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (setq cae-cheatsheets-minibuffer--last-hydra hydra-curr-body-fn
          cae-cheatsheets-minibuffer--last-workspace (and (featurep 'persp-mode)
                                                          (get-current-persp))
          cae-cheatsheets-minibuffer--last-tab (tab-bar--current-tab)
          cae-cheatsheets-minibuffer--last-tab-index (tab-bar--current-tab-index))
    (hydra-keyboard-quit)))

(defun cae-cheatsheets-minibuffer-hydra-resume-h (&rest _)
  (run-with-timer
   0.001 nil
   (lambda ()
     (when (and (featurep 'persp-mode)
                (not (eq (get-current-persp)
                         cae-cheatsheets-minibuffer--last-workspace)))
       (set-persp-parameter 'cae-cheatsheets-workspace--last-hydra
                            cae-cheatsheets-minibuffer--last-hydra
                            cae-cheatsheets-minibuffer--last-workspace))
     (when (not (and (equal (tab-bar--current-tab)
                            cae-cheatsheets-minibuffer--last-tab)
                     (eq (tab-bar--current-tab-index)
                         cae-cheatsheets-minibuffer--last-tab-index)))
       (setf (alist-get cae-cheatsheets-minibuffer--last-tab
                        cae-cheatsheets-tab-bar-hydra-alist
                        nil nil #'equal)
             cae-cheatsheets-minibuffer--last-hydra))
     (when (and cae-cheatsheets-minibuffer--last-hydra
                (equal (tab-bar--current-tab)
                       cae-cheatsheets-minibuffer--last-tab)
                (or (not (featurep 'persp-mode))
                    (eq (get-current-persp)
                        cae-cheatsheets-minibuffer--last-workspace)))
       (funcall cae-cheatsheets-minibuffer--last-hydra)
       (setq cae-cheatsheets-minibuffer--last-hydra nil)))))

(add-hook 'minibuffer-setup-hook #'cae-cheatsheets-minibuffer-hydra-pause-h)
(add-hook 'minibuffer-exit-hook #'cae-cheatsheets-minibuffer-hydra-resume-h)

(defvar cae-cheatsheets-workspace--last-hydra nil)

(defun cae-cheatsheets-workspace-hydra-pause-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (hydra-keyboard-quit)
    (set-persp-parameter 'cae-cheatsheets-workspace--last-hydra
                         hydra-curr-body-fn)))

(defun cae-cheatsheets-workspace-hydra-resume-h (&rest _)
  (when (persp-parameter 'cae-cheatsheets-workspace--last-hydra)
    ;; In my testing, using a timer prevented Hydra from clobbering my
    ;; workspace-switching repeat map.
    (run-with-timer 0.001 nil (persp-parameter 'cae-cheatsheets-workspace--last-hydra)))
  (set-persp-parameter 'cae-cheatsheets-workspace--last-hydra nil))

(add-hook 'persp-before-switch-functions
          #'cae-cheatsheets-workspace-hydra-pause-h)
(add-hook 'persp-activated-functions
          #'cae-cheatsheets-workspace-hydra-resume-h)

(defvar cae-cheatsheets-tab-bar-hydra-alist nil)

(defun cae-sheetsheets-tab-bar-store-hydra-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (setf (alist-get (tab-bar--current-tab)
                     cae-cheatsheets-tab-bar-hydra-alist
                     nil nil #'equal)
          hydra-curr-body-fn)
    (hydra-keyboard-quit)))

(defun cae-cheatsheets-tab-bar-resume-hydra-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (hydra-keyboard-quit))
  (when-let ((hydra (alist-get (tab-bar--current-tab)
                               cae-cheatsheets-tab-bar-hydra-alist
                               nil nil #'equal)))
    (setf (alist-get (tab-bar--current-tab)
                     cae-cheatsheets-tab-bar-hydra-alist
                     nil t #'equal)
          nil)
    (run-with-timer 0.001 nil hydra)))

(add-hook 'cae-tab-bar-before-switch-hook #'cae-sheetsheets-tab-bar-store-hydra-h)
(add-hook 'cae-tab-bar-after-switch-hook #'cae-cheatsheets-tab-bar-resume-hydra-h)

;; Make these persp-local so that identical tabs on different workspaces do not
;; share hydras.
(defun cae-cheatsheets-tab-bar-workspace-store-hydra-h (&rest _)
  (set-persp-parameter 'cae-cheatsheets-tab-bar-hydra-alist
                       cae-cheatsheets-tab-bar-hydra-alist))

(defun cae-cheatsheets-tab-bar-workspace-resume-hydra-h (&rest _)
  (setq cae-cheatsheets-tab-bar-hydra-alist
        (persp-parameter 'cae-cheatsheets-tab-bar-hydra-alist)))

(when (modulep! :ui workspaces)
  (add-hook 'persp-before-switch-functions #'cae-cheatsheets-tab-bar-workspace-store-hydra-h)
  (add-hook 'persp-activated-functions #'cae-cheatsheets-tab-bar-workspace-resume-hydra-h))
