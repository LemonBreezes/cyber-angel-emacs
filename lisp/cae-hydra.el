;;; lisp/cae-hydra.el -*- lexical-binding: t; -*-

;;; Save and resume hydras.

(defvar cae-hydra-minibuffer--last-hydra nil)
(defvar cae-hydra-minibuffer--last-workspace nil)
(defvar cae-hydra-minibuffer--last-tab nil)
(defvar cae-hydra-minibuffer--last-tab-index nil)

(defvar cae-hydra-tab--unique-identifier-fn
  (lambda ()
    (mapconcat #'buffer-name
               (cl-remove-if (lambda (buf)
                               (doom-unreal-buffer-p buf))
                (mapcar #'window-buffer
                        (window-list-1 (frame-first-window)
                                       'nomini)))
               ", ")))

(defun cae-hydra-minibuffer-pause-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (setq cae-hydra-minibuffer--last-hydra hydra-curr-body-fn
          cae-hydra-minibuffer--last-workspace (and (featurep 'persp-mode)
                                                          (get-current-persp))
          cae-hydra-minibuffer--last-tab (funcall cae-hydra-tab--unique-identifier-fn)
          cae-hydra-minibuffer--last-tab-index (tab-bar--current-tab-index))
    (hydra-keyboard-quit)))

(defun cae-hydra-minibuffer-resume-h (&rest _)
  (run-with-timer
   0.001 nil
   (lambda ()
     (when (and (featurep 'persp-mode)
                (not (eq (get-current-persp)
                         cae-hydra-minibuffer--last-workspace)))
       (set-persp-parameter 'cae-hydra-workspace--last-hydra
                            cae-hydra-minibuffer--last-hydra
                            cae-hydra-minibuffer--last-workspace))
     (when (not (and (equal (funcall cae-hydra-tab--unique-identifier-fn)
                            cae-hydra-minibuffer--last-tab)
                     (eq (tab-bar--current-tab-index)
                         cae-hydra-minibuffer--last-tab-index)))
       (setf (alist-get cae-hydra-minibuffer--last-tab
                        cae-hydra-tab-bar-hydra-alist
                        nil nil #'equal)
             cae-hydra-minibuffer--last-hydra))
     (when (and cae-hydra-minibuffer--last-hydra
                (equal (funcall cae-hydra-tab--unique-identifier-fn)
                       cae-hydra-minibuffer--last-tab)
                (or (not (featurep 'persp-mode))
                    (eq (get-current-persp)
                        cae-hydra-minibuffer--last-workspace)))
       (funcall cae-hydra-minibuffer--last-hydra)
       (setq cae-hydra-minibuffer--last-hydra nil))
     (setq cae-hydra-minibuffer--last-tab-index nil
           cae-hydra-minibuffer--last-tab nil
           cae-hydra-minibuffer--last-hydra nil
           cae-hydra-minibuffer--last-workspace nil))))

(add-hook 'minibuffer-setup-hook #'cae-hydra-minibuffer-pause-h)
(add-hook 'minibuffer-exit-hook #'cae-hydra-minibuffer-resume-h)

(defvar cae-hydra-workspace--last-hydra nil)

(defun cae-hydra-workspace-hydra-pause-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (hydra-keyboard-quit)
    (set-persp-parameter 'cae-hydra-workspace--last-hydra
                         hydra-curr-body-fn)))

(defun cae-hydra-workspace-hydra-resume-h (&rest _)
  (when (persp-parameter 'cae-hydra-workspace--last-hydra)
    (run-with-timer 0.001 nil (persp-parameter 'cae-hydra-workspace--last-hydra)))
  (set-persp-parameter 'cae-hydra-workspace--last-hydra nil))

(add-hook 'persp-before-switch-functions
          #'cae-hydra-workspace-hydra-pause-h)
(add-hook 'persp-activated-functions
          #'cae-hydra-workspace-hydra-resume-h)

(defvar cae-hydra-tab-bar-hydra-alist nil)

(defun cae-hydra-tab-bar-store-hydra-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (setf (alist-get (funcall cae-hydra-tab--unique-identifier-fn)
                     cae-hydra-tab-bar-hydra-alist
                     nil nil #'equal)
          hydra-curr-body-fn)
    (hydra-keyboard-quit)))

(defun cae-hydra-tab-bar-resume-hydra-h (&rest _)
  (when (bound-and-true-p hydra-curr-map)
    (hydra-keyboard-quit))
  (when-let ((hydra (alist-get (funcall cae-hydra-tab--unique-identifier-fn)
                               cae-hydra-tab-bar-hydra-alist
                               nil nil
                               #'equal)))
    (setf (alist-get (tab-bar-tab-name-all)
                     cae-hydra-tab-bar-hydra-alist
                     nil t #'equal)
          nil)
    (run-with-timer 0.001 nil hydra)))

(add-hook 'cae-tab-bar-before-switch-hook #'cae-hydra-tab-bar-store-hydra-h)
(add-hook 'cae-tab-bar-after-switch-hook #'cae-hydra-tab-bar-resume-hydra-h)

;; Make these persp-local so that identical tabs on different workspaces do not
;; share hydras.
(defun cae-hydra-tab-bar-workspace-store-hydra-h (&rest _)
  (set-persp-parameter 'cae-hydra-tab-bar-hydra-alist
                       cae-hydra-tab-bar-hydra-alist))

(defun cae-hydra-tab-bar-workspace-resume-hydra-h (&rest _)
  (setq cae-hydra-tab-bar-hydra-alist
        (persp-parameter 'cae-hydra-tab-bar-hydra-alist)))

(when (modulep! :ui workspaces)
  (add-hook 'persp-before-switch-functions #'cae-hydra-tab-bar-workspace-store-hydra-h)
  (add-hook 'persp-activated-functions #'cae-hydra-tab-bar-workspace-resume-hydra-h))
